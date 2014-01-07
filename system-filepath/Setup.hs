import qualified Distribution.Simple as Simple
import qualified Distribution.Simple.LocalBuildInfo as LocalBuildInfo
import qualified Distribution.Simple.Program as Program

main :: IO ()
main = Simple.defaultMainWithHooks hooks where
	hooks = Simple.simpleUserHooks
		{ Simple.haddockHook = myHaddockHook
		}
	myHaddockHook pkg lbi = Simple.haddockHook Simple.simpleUserHooks pkg (lbi
		{ LocalBuildInfo.withPrograms = Program.userSpecifyArgs "haddock" ["--optghc=-D__HADDOCK__"] (LocalBuildInfo.withPrograms lbi)
		})
