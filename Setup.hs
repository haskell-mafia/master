import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.BuildPaths
import Distribution.Verbosity

main = do defaultMainWithHooks simpleUserHooks {
    buildHook = \packageDescription localBuildInfo userHooks buildFlags -> do
      rawSystemExit normal "touch" ["src/Master/TH.hs"]
      buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
  }
