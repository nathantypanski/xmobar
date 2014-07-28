{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The configuration module of Xmobar, a text based status bar
--
-----------------------------------------------------------------------------

module Config
    ( -- * Configuration
      -- $config
      Config (..)
    , XPosition (..), Align (..), Border(..)
    , defaultConfig
    , runnableTypes
    ) where


import Commands
import {-# SOURCE #-} Runnable
import Plugins.Monitors
import Plugins.Date
import Plugins.PipeReader
import Plugins.BufferedPipeReader
import Plugins.CommandReader
import Plugins.StdinReader
import Plugins.XMonadLog
import Plugins.EWMH
import Plugins.Kbd
import Plugins.Locks

import Plugins.Mail
import Plugins.MBox

import Plugins.DateZone

-- $config
-- Configuration data type and default configuration

-- | The configuration data type
data Config =
    Config { font :: String         -- ^ Font
           , bgColor :: String      -- ^ Backgroud color
           , fgColor :: String      -- ^ Default font color
           , position :: XPosition  -- ^ Top Bottom or Static
           , border :: Border       -- ^ NoBorder TopB BottomB or FullB
           , borderColor :: String  -- ^ Border color
           , hideOnStart :: Bool    -- ^ Hide (Unmap) the window on
                                    --   initialization
           , allDesktops :: Bool    -- ^ Tell the WM to map to all desktops
           , overrideRedirect :: Bool -- ^ Needed for dock behaviour in some
                                      --   non-tiling WMs
           , lowerOnStart :: Bool   -- ^ lower to the bottom of the
                                    --   window stack on initialization
           , persistent :: Bool     -- ^ Whether automatic hiding should
                                    --   be enabled or disabled
           , commands :: [Runnable] -- ^ For setting the command,
                                    --   the command arguments
                                    --   and refresh rate for the programs
                                    --   to run (optional)
           , sepChar :: String      -- ^ The character to be used for indicating
                                    --   commands in the output template
                                    --   (default '%')
           , alignSep :: String     -- ^ Separators for left, center and
                                    -- right text alignment
           , template :: String     -- ^ The output template
           } deriving (Read)

data XPosition = Top
               | TopW Align Int
               | TopSize Align Int Int
               | TopP Int Int
               | Bottom
               | BottomP Int Int
               | BottomW Align Int
               | BottomSize Align Int Int
               | Static {xpos, ypos, width, height :: Int}
               | OnScreen Int XPosition
                 deriving ( Read, Eq )

data Align = L | R | C deriving ( Read, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
              deriving ( Read, Eq )

-- | The default configuration values
defaultConfig :: Config
defaultConfig =
    Config { font = "-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso8859-*"
           , border = NoBorder
           , borderColor = "#BFBFBF"
           , hideOnStart = False
           , lowerOnStart = True
           , persistent = False
           , allDesktops = True
           , bgColor = "#151515"
           , fgColor = "#D7D0C7"
           , position = TopW L 100
           , overrideRedirect = False
           , commands = [ Run $ Weather "EGPF"
                            [
                             "-t"
                             ," <tempF>F"
                             ,"-L"
                             ,"64"
                             ,"-H"
                             ,"77"
                             ,"--normal","#859900"
                             ,"--high","#dc322f"
                             ,"--low","#268bd2"
                            ] 36000
                        , Run $ DiskIO [("sda1", "<read>:<write>")] [] 10
                        , Run $ Cpu ["-L","3","-H","50","--normal","#859900","--high","#dc322f"] 100
                        , Run $ Memory ["-t","Mem: <usedratio>%"] 100
                        , Run $ Swap [] 100
                        , Run $ Date "%a %b %_d %l:%M" "date" 10
                        , Run $ Com "/home/nathan/bin/batp" [] "" 600
                        , Run StdinReader
                        ]
           , sepChar = "%"
           , alignSep = "}{"
           , template = "%StdinReader% }{ %diskio% | %cpu% | %memory% * %swap%    <fc=#ee9a00>%date%</fc> | %EGPF% %batp%"
           }

-- | An alias for tuple types that is more convenient for long lists.
type a :*: b = (a, b)
infixr :*:

-- | This is the list of types that can be hidden inside
-- 'Runnable.Runnable', the existential type that stores all commands
-- to be executed by Xmobar. It is used by 'Runnable.readRunnable' in
-- the 'Runnable.Runnable' Read instance. To install a plugin just add
-- the plugin's type to the list of types (separated by ':*:') appearing in
-- this function's type signature.
runnableTypes :: Command :*: Monitors :*: Date :*: PipeReader :*: BufferedPipeReader :*: CommandReader :*: StdinReader :*: XMonadLog :*: EWMH :*: Kbd :*: Locks :*:
                 Mail :*: MBox :*:
                 DateZone :*:
                 ()
runnableTypes = undefined
