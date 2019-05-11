{-# OPTIONS_GHC -Wno-missing-signatures #-}
import System.Exit
import qualified Data.Map.Internal
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.FixedColumn
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
    ("M-,",         sendMessage (IncMasterN 1)),
    ("M-.",         sendMessage (IncMasterN (-1))),
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
        | (screen, key) <- zip [0..] "qwe",
        (mask, action) <- [("", W.view), ("S-", W.shift)]
    ]

tall = Tall 1 (3/100) (1/2)
layouts = tall ||| Mirror tall ||| ThreeCol 1 (3/100) (1/2) ||| (R.renamed [R.Replace "ThreeColMid"] $ ThreeColMid 1 (3/100) (1/2)) ||| Full
inner = Simplest
outer = boringWindows layouts

defaultLayout = configurableNavigation (navigateBrightness 0) $ addTabs shrinkText myTheme $ subLayout [] inner $ outer
--defaultLayout = configurableNavigation (navigateBrightness 0) $ (addTabs shrinkText myTheme outer) ||| subLayout [] inner outer
--defaultLayout = configurableNavigation (navigateBrightness 0) $ addTabs shrinkText myTheme outer

irssiLayout = configurableNavigation (navigateBrightness 0) $ addTabs shrinkText myTheme $ subLayout [] Simplest $ boringWindows $ FixedColumn 1 14 132 20

layout = onWorkspace "irssi" irssiLayout $
    defaultLayout

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
    layoutHook = layout,
    borderWidth = 4,
    workspaces = myWorkspaces,
    normalBorderColor = color!!2,
    focusedBorderColor = color!!7
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
