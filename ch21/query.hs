import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC

query :: Int -> IO ()
query maxId = do
    conn <- connectSqlite3 "test1.db"
    r <- quickQuery' conn
        "SELECT id, desc from test where id <= ? order by id, desc"
        [toSql (maxId::Int)]
    let stringRows = map convRow r
    mapM_ putStrLn stringRows
    disconnect conn
    where
        convRow :: [SqlValue] -> String
        convRow [sqlId, sqlDesc] =
            show intid ++ ": " ++ desc
            where
                intid = (fromSql sqlId) :: Integer
                desc = case fromSql sqlDesc of
                    Just x -> x
                    Nothing -> "NULL"
        convRow x = fail $ "Unexpected result: " ++ show x
