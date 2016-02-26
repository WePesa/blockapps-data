
module Blockchain.DB.MemAddressStateDB (
  getAddressState,
  getAllAddressStates,
  putAddressState,
  deleteAddressState,
  addressStateExists
) where 

import qualified Blockchain.DB.AddressStateDB as DB
import Blockchain.Data.Address
import Blockchain.Data.AddressStateDB
import Blockchain.DB.StateDB
import Blockchain.DB.HashDB

getAddressState::(HasStateDB m, HasHashDB m)=>
                 Address->m AddressState
getAddressState address = DB.getAddressState address
        
getAllAddressStates::(HasHashDB m, HasStateDB m)=>
                     m [(Address, AddressState)]
getAllAddressStates = DB.getAllAddressStates

putAddressState::(HasStateDB m, HasHashDB m)=>
                 Address->AddressState->m ()
putAddressState address newState = DB.putAddressState address newState

deleteAddressState::HasStateDB m=>Address->
                    m ()
deleteAddressState address = DB.deleteAddressState address

addressStateExists::HasStateDB m=>Address->
                    m Bool
addressStateExists address = DB.addressStateExists address
