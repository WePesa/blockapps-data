
module Blockchain.DB.MemAddressStateDB (
  HasMemAddressStateDB(..),
  getAddressState,
  getAllAddressStates,
  putAddressState,
  deleteAddressState,
  addressStateExists
) where 

import qualified Data.Map as M

import qualified Blockchain.DB.AddressStateDB as DB
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.DB.StateDB
import Blockchain.DB.HashDB

class HasMemAddressStateDB m where
  getAddressStateDBMap::m (M.Map Address AddressState)
  putAddressStateDBMap::M.Map Address AddressState->m ()


getAddressState::(HasMemAddressStateDB m, HasStateDB m, HasHashDB m)=>
                 Address->m AddressState
getAddressState address = DB.getAddressState address
        
getAllAddressStates::(HasMemAddressStateDB m, HasHashDB m, HasStateDB m)=>
                     m [(Address, AddressState)]
getAllAddressStates = DB.getAllAddressStates

putAddressState::(HasMemAddressStateDB m, HasStateDB m, HasHashDB m)=>
                 Address->AddressState->m ()
putAddressState address newState = DB.putAddressState address newState

deleteAddressState::(HasMemAddressStateDB m, HasStateDB m)=>Address->
                    m ()
deleteAddressState address = DB.deleteAddressState address

addressStateExists::(HasMemAddressStateDB m, HasStateDB m)=>Address->
                    m Bool
addressStateExists address = DB.addressStateExists address
