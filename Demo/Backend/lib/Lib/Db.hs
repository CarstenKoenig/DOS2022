module Lib.Db
  ( DbConnection,
    create,
    prepareDbProd,
    prepareDbTest,
  )
where

import Lib.Db.DbConnection (DbConnection, create)
import Lib.Db.Schema (prepareDbProd, prepareDbTest)