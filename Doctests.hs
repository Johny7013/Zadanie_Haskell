module Tests where
import Hashable32
import HashTree
import Blockchain
import PPrint

{- | Tree building
>>> putStr $ drawTree $ buildTree "fubar"
0x2e1cc0e4 -
  0xfbfe18ac -
    0x6600a107 -
      0x00000066 'f'
      0x00000075 'u'
    0x62009aa7 -
      0x00000062 'b'
      0x00000061 'a'
  0xd11bea20 +
    0x7200b3e8 +
      0x00000072 'r'
-}

{- | Tree printing
>>> print $ drawTree $ buildTree "a"
"0x00000061 'a'\n"
-}

{- | Proof building
>>> buildProof 'i' $ buildTree "bitcoin"
Just (MerkleProof 'i' <0x5214666a<0x7400b6ff>0x00000062)

>>> buildProof 'e' $ buildTree "bitcoin"
Nothing

>>> buildProof 'a' $ buildTree "a"
Just (MerkleProof 'a' )
-}

{- | Paths building
>>> mapM_ print $ map showMerklePath  $ merklePaths 'i' $ buildTree "bitcoin"
"<0x5214666a<0x7400b6ff>0x00000062"
">0x69f4387c<0x6e00ad98>0x0000006f"

>>> merklePaths 'i' $ buildTree "bitcoin"
[[Left 1377068650,Left 1946203903,Right 98],[Right 1777612924,Left 1845538200,Right 111]]
-}

{- | Proof verifying
>>> let t = buildTree "bitcoin"
>>> let proof = buildProof 'i' t
>>> verifyProof (treeHash t) <$> proof
Just True
>>> verifyProof 0xbada55bb <$> proof
Just False
-}

{- | Block mining
>>> mineBlock (hash "Charlie") (hash block1) [tx1]
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
<BLANKLINE>
-}

-- | Chain verification
-- >>> verifyChain [block1, block2]
-- Nothing
--
-- >>> VH <$> verifyChain [block2,block1,block0]
-- Just 0x0dbea380

{- | Block verification
>>> verifyBlock block0 0
Just 1890857696
>>> verifyBlock block1 1890857696
Just 797158976
>>> verifyBlock block2 797158976
Just 230597504

>>> let badBlockHdr2 = (blockHdr block2) { nonce = 42 }
>>> let badBlock2 = block2 { blockHdr = badBlockHdr2 }
>>> verifyBlock badBlock2 797158976
Nothing
-}

{- | Transaction Receipts
>>> charlie = hash "Charlie"
>>> let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
>>> block
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
<BLANKLINE>

>>> receipt
TxReceipt {txrBlock = 230597504, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0xbcc3e45a}
>>> validateReceipt receipt (blockHdr block)
True

>>> makeTx f t a = Tx (hash f) (hash t) (a*coin)
>>> let tx1 = makeTx "Satoshi" "Alice" 10
>>> let tx2 = makeTx "Alice" "Bob" 1
>>> let tx3 = makeTx "Alice" "Charlie" 1
>>> mineTransactions (hash "Charlie") (hash block1) [tx1,tx2,tx3]
(BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2996394280, nonce = 26}
Tx {txFrom = 1912855007, txTo = 2030195168, txAmount = 10000}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
Tx {txFrom = 2030195168, txTo = 1392748814, txAmount = 1000}
,[TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 1912855007, txTo = 2030195168, txAmount = 10000}) <0xae9d56b7>0xbcc3e45a},TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0x3c177e6b<0x1b6a0892},TxReceipt {txrBlock = 3725795968, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 1392748814, txAmount = 1000}) >0x3c177e6b>0x085e2467}])
-}

{- | Pretty printing
>>> runShows $ pprBlock block2
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000

>>> runShows $ pprListWith pprBlock [block0, block1, block2]
hash: 0x70b432e0
parent: 0000000000
miner: 0x7203d9df
root: 0x5b10bd5d
nonce: 18
Tx# 0x5b10bd5d from: 0000000000 to: 0x7203d9df amount: 50000
hash: 0x2f83ae40
parent: 0x70b432e0
miner: 0x790251e0
root: 0x5ea7a6f0
nonce: 0
Tx# 0x5ea7a6f0 from: 0000000000 to: 0x790251e0 amount: 50000
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000

>>> runShows $ pprBlock $ mineBlock (hash "Charlie") (hash block1) [tx1]
hash: 0x0dbea380
parent: 0x2f83ae40
miner: 0x5303a90e
root: 0x8abe9e15
nonce: 3
Tx# 0xbcc3e45a from: 0000000000 to: 0x5303a90e amount: 50000
Tx# 0x085e2467 from: 0x790251e0 to: 0xb1011705 amount: 1000
-}
