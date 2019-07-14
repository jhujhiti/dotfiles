{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-
-- Quick guide to key bindings

Applications: M-S-<key>
b - firefox
p - keepassxc
c - calculator
a - emacs
v - gvim
<Return> - urxvt

Windows:
Generally, M alone is focus management. M-S is window management. M-C is tab management.
M-[hjkl] - move focus in that direction
M-[;'] - move focus left or right in this tab group
M-[qwer] - move focus to screen
M-S-[hjkl] - swap window with the one in that direction
M-S-[;'] - grow or shrink the master area
M-S-[,.] - grow or shrink the number of master windows
M-S-[qwer] - move window to screen
M-C-[hjkl] - grab the window in that direction and pull it into a tab group with this one
M-C-m - merge all windows into tab group
M-C-u - unmerge this window from the tab group
M-M1-S-c - kill

Workspaces:
M-[`1234567890] - switch to workspace
M-S-[`1234567890] - move window to workspace
M-<Tab> - toggle between workspaces (like alt-tab for workspaces)
M-[nm] - previous/next workspace

M-b - toggle struts
M-z - restart xmonad
M-M1-S-z - quit xmonad
-}
import System.Exit
import qualified Data.Map.Internal
import XMonad hiding ((|||))
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.FixedColumn
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Util.EZConfig
import qualified XMonad.Layout.Renamed as R
import qualified XMonad.StackSet as W

workspaceKeys :: String
workspaceKeys = "`1234567890"

myWorkspaces :: [String]
myWorkspaces = ["irssi"] ++ map (\x -> [x]) "1234567890"

screenKeys :: String
screenKeys = "qwer"

myTerminal :: String
myTerminal = "urxvt"

appKeys :: [(String, String)]
appKeys = [
    ("<Return>", myTerminal),
    ("v", "gvim"),
    ("c", "galculator"),
    ("p", "keepassxc ~/storage/passwords.kdbx"),
    ("b", "firefox"),
    ("a", "emacs")
    ]

applications :: [(String, String)] -> [(String, X ())]
applications = map (\(k, v) -> ("M-S-" ++ k, spawn v))

-- perform a given WindowNavigation action in hjkl directions
withDirections :: String -> (Direction2D -> Navigate) -> [(String, X ())]
withDirections prefix action = map f directions
    where f :: (Direction2D, String) -> (String, X ())
          f (d, k) = (prefix ++ "-" ++ k, sendMessage $ action d)
          directions :: [(Direction2D, String)]
          directions = [(L, "h"), (R, "l"), (U, "k"), (D, "j")]

autoKeys :: [(String, X ())]
autoKeys = foldl1 (++) l
    where l = [
              applications appKeys,
              withDirections "M" Go,
              withDirections "M-S" Swap,
              withDirections "M-C" pullGroup
              ]

myKeys :: XConfig Layout -> Data.Map.Internal.Map (KeyMask, KeySym) (X())
myKeys = \conf -> mkKeymap conf $ autoKeys ++
    [
    -- media keys
    ("<XF86MonBrightnessUp>",   spawn "xbacklight +5"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -5"),
    ("<XF86AudioMute>",         spawn "pamixer -t"),
    ("<XF86AudioRaiseVolume>",  spawn "pamixer -i 5"),
    ("<XF86AudioLowerVolume>",  spawn "pamixer -d 5"),
    -- window management
    ("M-<Space>",   sendMessage NextLayout),
    ("M-S-<Space>", setLayout $ layoutHook conf),
    -- submap for selecting specific layouts
    ("M-C-<Space> 1", sendMessage $ JumpToLayout "Tall"),
    ("M-C-<Space> 2", sendMessage $ JumpToLayout "Mirror Tall"),
    ("M-C-<Space> 3", sendMessage $ JumpToLayout "ThreeCol"),
    ("M-C-<Space> 4", sendMessage $ JumpToLayout "ThreeColMid"),
    ("M-C-<Space> 5", sendMessage $ JumpToLayout "Full"),
    ("M1-<Tab>",    windows W.focusDown),
    ("M1-S-<Tab>",  windows W.focusUp),
    ("M-S-m",       windows W.focusMaster),
    ("M-<Return>",  windows W.swapMaster),
    ("M-C-m",       withFocused $ sendMessage . MergeAll),
    ("M-C-u",       withFocused $ sendMessage . UnMerge),
    ("M-;",         onGroup W.focusUp'),
    ("M-S-;",       sendMessage Shrink),
    ("M-'",         onGroup W.focusDown'),
    ("M-S-'",       sendMessage Expand),
    ("M-t",         withFocused $ windows . W.sink),
    ("M-S-,",       sendMessage (IncMasterN 1)),
    ("M-S-.",       sendMessage (IncMasterN (-1))),
    ("M-n",         prevWS),
    ("M-m",         nextWS),
    ("M-<Tab>",     toggleWS),
    ("M-M1-S-c",    kill),
    ("M-z",         restart "xmonad" True),
    ("M-M1-S-z",    io (exitWith ExitSuccess))
    ]
    ++
    [(mask ++ "M-" ++ [key], action tag)
        | (tag, key) <- zip myWorkspaces workspaceKeys,
        (mask, action) <- [("", windows . W.view), ("S-", windows . W.shift)]
    ]
    ++
    [(mask ++ "M-" ++ [key], screenWorkspace screen >>= flip whenJust (windows . action))
        | (screen, key) <- zip [0..] screenKeys,
        (mask, action) <- [("", W.view), ("S-", W.shift)]
    ]

tall = Tall 1 (3/100) (1/2)
layouts = tall ||| Mirror tall ||| ThreeCol 1 (3/100) (1/2) ||| (R.renamed [R.Replace "ThreeColMid"] $ ThreeColMid 1 (3/100) (1/2)) ||| Full
inner = Simplest
outer = boringWindows layouts

addnav l = configurableNavigation (navigateBrightness 0) l

-- add tabs to the layout without adding the "Tabbed" to the layout description
addtabs l = R.renamed [R.CutWordsLeft 1] $ addTabs shrinkText myTheme l

defaultLayout = addnav $ addtabs $ subLayout [] inner $ outer

irssiLayout = addnav $ addtabs $ subLayout [] inner $ boringWindows $ FixedColumn 1 14 132 20

layout = onWorkspace "irssi" irssiLayout $
    defaultLayout

myManageHook = composeAll [
-- TODO
-- Allows focusing other monitors without killing the fullscreen
--  [ isFullscreen --> (doF W.focusDown <+> doFullFloat)
  isFullscreen --> doFullFloat
  ]

-- this is based on base16-eighties
color :: [String]
color = [
    "#2d2d2d",
    "#393939",
    "#515151",
    "#747369",
    "#a09f93",
    "#d3d0c8",
    "#e8e6df",
    "#f2f0ec",
    "#f2777a",
    "#f99157",
    "#ffcc66",
    "#99cc99",
    "#66cccc",
    "#6699cc",
    "#cc99cc",
    "#d27b53"
    ]

myTheme = def {
    activeColor = color!!2,
    inactiveColor = color!!0,
    urgentColor = color!!0,
    activeBorderColor = color!!7,
    inactiveBorderColor = color!!0,
    urgentBorderColor = color!!8,
    activeTextColor = color!!5,
    inactiveTextColor = color!!5,
    urgentTextColor = color!!5,
    fontName = "xft:DejaVu Sans-8,antialias=true",
    decoHeight = 30
    }

xmonadConfig = withUrgencyHook NoUrgencyHook $ def {
    modMask = mod4Mask,
    terminal = myTerminal,
    keys = myKeys,
    layoutHook = smartBorders (layout),
    borderWidth = 4,
    workspaces = myWorkspaces,
    normalBorderColor = color!!2,
    focusedBorderColor = color!!7,
    manageHook = myManageHook <+> manageHook def
    }

myXmobarPP = xmobarPP {
    ppTitle = shorten 100,
    ppCurrent = wrap "[" "]",
    ppVisible = wrap "(" ")",
    ppUrgent = xmobarColor (color!!8) "",
    ppSep = " | "
    }

toggleStrutsKey XConfig { XMonad.modMask = mask } = (mask, xK_b)

main = do
    xmonad =<< statusBar "xmobar" myXmobarPP toggleStrutsKey xmonadConfig
