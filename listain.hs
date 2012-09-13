
import System.IO
import System.Directory
import Control.Exception
import Data.List
import Data.List.Utils
import Network
import Network.Socket (sIsListening)
import Text.Printf
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Concurrent
import Control.Monad
import System.Posix.Signals


type ListId = Int
type ListName = String
type ItemId = Int

data Item = Item { item_id :: ItemId, text :: String }
data List = List { list_id :: ListId, items :: [Item] }
type Lists = [(ListId, ListName)]
data ListPos = ListBottom | ListTop


listfile = "list.lst"
dbfile = "listain.db"
greeting = "LISTAIN 001"

logf :: HPrintfType r => String -> r
logf = hPrintf stderr


encodeItem :: Item -> String
encodeItem (Item id text) = printf "%d %s" id text

decodeItem :: String -> Item
decodeItem string =
	Item id text
	where
		(id_str, text) = cut string
		id = read id_str


encodeList :: List -> String
encodeList list =
	unlines_with_len (map encodeItem (items list))


encodeLists :: Lists -> String
encodeLists lists =
	unlines_with_len (map (\(id, name) -> printf "%d %s" id name) lists)


unlines_with_len :: [String] -> String
unlines_with_len lines =
	unlines $ len_line : lines
	where
		len_line = printf "LEN %d" $ length lines


itemCount :: List -> Int
itemCount list = length $ items list


cut :: String -> (String, String)
cut string =
	(first, rest)
	where
		parts = words string
		(first, rest) = if parts == []
			then ("", "")
			else (head (words string), drop (length first + 1) string)


initDB :: IO Connection
initDB = do
	db <- connectSqlite3 dbfile
	run db "PRAGMA foreign_keys = ON" []
	run db "CREATE TABLE IF NOT EXISTS lists (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)" []
	run db "CREATE TABLE IF NOT EXISTS items (id INTEGER NOT NULL PRIMARY KEY, list INTEGER NOT NULL, text VARCHAR(255) NOT NULL, FOREIGN KEY (list) REFERENCES lists(id))" []
	commit db
	return db


main :: IO ()
main = do
	
	db <- initDB
	withSocketsDo $ do
		let port = PortNumber 11511
		s <- listenOn port
		logf "Listening.\n" 
		finally (listenLoop db s) (sClose s)


listenLoop :: Connection -> Socket -> IO ()
listenLoop db s = do
	-- listening <- sIsListening s
--	if listening
	(h, host, port) <- accept s
	let worker = do
		let port_number = fromIntegral port :: Int
		hSetBuffering h LineBuffering
		logf "Accepted connection: %s:%d.\n" host port_number
		hPutStrLn h greeting
		finally (process db h host port) (hClose h)
		logf "Closed connection: %s:%d.\n" host port_number
	forkIO worker
	listenLoop db s


loadLists :: Connection -> IO Lists
loadLists db = do
	results <- quickQuery' db "SELECT id, name FROM lists" []
	return $ map processRow results
	where
		processRow :: [SqlValue] -> (ListId, ListName)
		processRow [id, name] = (fromSql id, fromSql name)


loadList :: Connection -> ListId -> IO List
loadList db list = do
	results <- quickQuery' db "SELECT id, text FROM items WHERE list = ?" [toSql list]
	return $ List list (map processRow results)
	where
		processRow [id, text] = Item (fromSql id) (fromSql text)


addList :: Connection -> ListName -> IO ()
addList db name = do
	quickQuery' db "INSERT INTO lists (name) VALUES (?)" [toSql name]
	commit db
	return ()


addItem :: Connection -> ListId -> ListPos -> String -> IO ()
addItem db list pos text = do
	quickQuery' db "INSERT INTO items (list, text) VALUES (?, ?)" [toSql list, toSql text]
	commit db
	return ()


process :: Connection -> Handle -> HostName -> PortNumber -> IO ()
process db h host port = do
	eof <- hIsEOF h
	if not eof then do
		(cmd, rest) <- liftM (cut . stripLinefeed) $ hGetLine h
		logf "Received command '%s' with parameters '%s'.\n" cmd rest
		react db h cmd rest
		process db h host port
	else
		return ()


stripLinefeed :: String -> String
stripLinefeed = reverse . dropWhile (== '\r') . reverse


react :: Connection -> Handle -> String -> String -> IO ()
react db h cmd rest =
	case cmd of
	"LISTS" -> do
		lists <- loadLists db
		hPutStr h (encodeLists lists)
	"LIST" -> do
		let id = read rest
		list <- loadList db id
		hPutStr h (encodeList list)
	"ADDLIST" -> do
		addList db rest
		hPutStrLn h "DONE"
	"ADDITEMBOT" -> do
		let (list_str, text) = cut rest
		let list = read list_str
		addItem db list ListBottom text
		hPutStrLn h "DONE"
	_ -> do
		hPutStrLn h "FAIL"


