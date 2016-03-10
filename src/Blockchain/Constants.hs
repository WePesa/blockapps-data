{-# OPTIONS_GHC  -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Blockchain.Constants where


ethVersion::Integer
ethVersion=61
shhVersion::Integer
shhVersion=2

_Uether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000;
_Vether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000;
_Dether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000;
_Nether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000000;
_Yether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000000;
_Zether = 1000000000 * 1000000000 * 1000000000 * 1000000000 * 1000;
_Eether = 1000000000 * 1000000000 * 1000000000 * 1000000000;
_Pether = 1000000000 * 1000000000 * 1000000000 * 1000000;
_Tether = 1000000000 * 1000000000 * 1000000000 * 1000
_Gether = 1000000000 * 1000000000 * 1000000000
_Mether = 1000000000 * 1000000000 * 1000000
_Kether = 1000000000 * 1000000000 * 1000
ether = 1000000000000000000
finney = 1000000000000000
szabo = 1000000000000
_Gwei = 1000000000
_Mwei = 1000000
_Kwei = 1000
wei = 1

--------

minimumDifficulty=131072

difficultyDurationLimit testnet =if testnet then 8 else 13

difficultyAdjustment=11::Int

difficultyExpDiffPeriod=100000

minGasLimit testnet = if testnet then 125000 else 5000

rewardBase testnet = if testnet then 1500 * finney else 5000 * finney

-------------


stateDBPath::String
stateDBPath="/state/"

hashDBPath::String
hashDBPath="/hash/"

codeDBPath::String
codeDBPath="/code/"

dbDir::String->String
dbDir "c" = ".ethereum"
--dbDir "c" = "Library/Application Support/Ethereum"
dbDir "h" = ".ethereumH"
dbDir "t" = "/tmp/tmpDB"
dbDir x = error $ "Unknown DB specifier: " ++ show x


--"/Users/hutong/Library/Application Support/Ethereum/state/"
