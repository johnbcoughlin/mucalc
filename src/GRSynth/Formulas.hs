module GRSynth.Formulas ( realize
                        , Realization
                        , RealizationError (UnknownAtomError)
                        , SimpleFormula (Atom, Negation, Or, And)
                        ) where

import GRSynth.States
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Map as M

data SimpleFormula = Atom String | Negation SimpleFormula |
                     Or SimpleFormula SimpleFormula | And SimpleFormula SimpleFormula

type Model = M.Map String StateSet
type RealizationM = ReaderT Model (ErrorT RealizationError Identity) StateSet

data RealizationError = UnknownAtomError
                      deriving (Show, Eq)

instance Error RealizationError

type Realization = Either RealizationError StateSet

realize :: Model -> SimpleFormula -> Realization
realize m f = runRealize (doRealize f) m

runRealize :: RealizationM -> Model -> Realization
runRealize comp model = runIdentity (runErrorT (runReaderT comp model))

doRealize :: SimpleFormula -> RealizationM

doRealize (Atom p) = do model <- ask
                        case M.lookup p model of
                             Just s -> return s
                             Nothing -> throwError UnknownAtomError

doRealize (Negation f) = do rf <- doRealize f
                            return $ setNot rf

doRealize (Or f1 f2) = do rf1 <- doRealize f1
                          rf2 <- doRealize f2
                          return $ rf1 `setOr` rf2

doRealize (And f1 f2) = do rf1 <- doRealize f1
                           rf2 <- doRealize f2
                           return $ rf1 `setAnd` rf2
