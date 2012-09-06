
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


type ListId = Int
type ListName = String
type ItemId = Int

data Item = Item { item_id :: ItemId, text :: String }
data List = List { list_id :: ListId, items :: [Item] }
type Lists = [(ListId, ListName)]


listfile = "list.lst"
dbfile = "listain.db"

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
encodeList (List items id_counter) = 
	unlines (id_line : rest)
	where
		id_line = show id_counter
		rest = map encodeItem items


decodeList :: String -> List
decodeList string =
	List items id_counter
	where
		(id_line : item_lines) = (filter (not.null) . lines) string
		items = map decodeItem item_lines
		id_counter = read id_line


encodeLists :: Lists -> String
encodeLists lists =
	unlines (map (\(id, name) -> printf "%d %s" id name) lists)


itemCount :: List -> Int
itemCount (List items _) = length items


cut :: String -> (String, String)
cut string =
	(first, rest)
	where
		first = head (words string)
		rest = drop (length first + 1) string


initDB :: IO Connection
initDB = do
	db <- connectSqlite3 dbfile
	run db "PRAGMA foreign_keys = ON"
	run db "CREATE TABLE IF NOT EXISTS lists (id INTEGER NOT NULL, name VARCHAR(50) NOT NULL, PRIMARY KEY (id))" []
	run db "CREATE TABLE IF NOT EXISTS items (list INTEGER NOT NULL, id INTEGER NOT NULL, text VARCHAR(255) NOT NULL, PRIMARY KEY (list, id), FOREIGN KEY (list) REFERENCES lists(id))" []
	commit db
	return db


save :: List -> IO ()
save list = do
	(tempfile, temph) <- openTempFile "." (listfile ++ ".tmp")
	logf "Opened temporary file %s.\n" tempfile
	finally
		(hPutStrLn temph $ encodeList list)
		(hClose temph)
	renameFile tempfile listfile
	logf "Saved list with %d items.\n" (itemCount list)


load :: IO List
load = do
	h <- openFile listfile ReadMode
	contents <- hGetContents h
	let list = decodeList contents
	logf "Loaded list with %d items.\n" (itemCount list)
	return list


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
	listening <- sIsListening s
	if listening
		then do
			(h, host, port) <- accept s
			logf "Accepted connection from %s.\n" host
			-- list <- load
			finally (process db h host port) (hClose h)
			listenLoop s
		else
			return ()


loadLists :: Connection -> IO [(ListId, ListName)]
loadLists db = do
	results <- quickQuery "SELECT FROM lists (id, name)" []
	return $ map processRow results
	where
		processRow :: [SqlValue] -> (ListId, ListName)
		processRow [id, name] = (fromSql id, fromSql name)


process :: Connection -> Handle -> HostName -> PortNumber -> IO ()
process db h host port = do
	line <- hGetLine h
	let (cmd, rest) = cut line
	logf "Received command '%s' with parameters '%s'.\n" cmd rest
	react db h cmd rest


react :: Connection -> Handle -> String -> String -> IO (Maybe List)
react db h list cmd rest
	| cmd == "LISTS" = do
		lists <- loadLists
		hPutStrLn h $ encodeLists lists
		return Nothing
	| otherwise = do
		hPutStrLn h "FAIL"
		return Nothing

