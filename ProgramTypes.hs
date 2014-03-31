module ProgramTypes where

data Settings = Settings {
	port :: Maybe Port
}
	deriving( Show )

type Port = Int

data State = State {
}
