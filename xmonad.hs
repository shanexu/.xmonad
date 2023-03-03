import           Control.Monad              (when)
import qualified Data.Text                  as T
import           Data.Text.Encoding.Error   (ignore)
import qualified DBus.Client                as DC
import           Network.HostName
import           System.Directory
import           XMonad                     (XConfig (borderWidth, focusFollowsMouse, focusedBorderColor, handleEventHook, layoutHook, logHook, manageHook, modMask, normalBorderColor, startupHook, terminal, workspaces),
                                             className, composeAll, controlMask,
                                             doF, doFloat, doIgnore, ifM,
                                             mod1Mask, mod4Mask, resource,
                                             screenWorkspace, sendMessage,
                                             shiftMask, spawn, title, whenJust,
                                             windows, withFocused, xK_0, xK_F2,
                                             xK_a, xK_backslash, xK_c, xK_e,
                                             xK_g, xK_l, xK_m, xK_p, xK_r, xK_v,
                                             xK_w, xK_x, xmonad, (-->), (.|.),
                                             (<&&>), (<+>), (=?), (|||))
import           XMonad.Actions.CopyWindow  (copyToAll)
import           XMonad.Actions.NoBorders
import           XMonad.Config.Desktop
import           XMonad.Config.Gnome
import qualified XMonad.DBus                as D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat,
                                             isFullscreen)
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Maximize
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet            as W
import           XMonad.Util.EZConfig
import           XMonad.Util.SpawnOnce
import           XMonad.Util.Themes         (ThemeInfo (theme))

main :: IO ()
main = do
  hostname <- getHostName
  dbus <- D.connect
  D.requestAccess dbus
  xmonad $ ewmhFullscreen $ gnomeConfig
    { modMask = myModMask
    , logHook = dynamicLogWithPP (myLogHook dbus) <+> logHook gnomeConfig
    -- , terminal = "env GLFW_IM_MODULE=ibus kitty"
    -- , terminal = "wezterm"
    , terminal = "tabbed alacritty --embed"
    , workspaces = myWorkspaces
    , borderWidth = 6
    , focusFollowsMouse = True
    , layoutHook = myLayout
    -- , normalBorderColor = "#707880"
    -- , focusedBorderColor = "#33AADD"
    , normalBorderColor = "#555555"
    , focusedBorderColor = "#f36864"
    , handleEventHook = handleEventHook gnomeConfig
    , startupHook = myStartupHook hostname
    , manageHook = myManageHook <+> manageHook gnomeConfig
    } `additionalKeys` (
    [((myModMask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces ]
    ++ [((myModMask .|. shiftMask, key), windows $ W.shift ws) | (key, ws) <- myExtraWorkspaces]
    ++ [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [2,0,1] -- was [0..] *** change to match your screen order ***
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++ [
        ((myModMask, xK_g), withFocused toggleBorder)
       ,((myModMask, xK_p), spawn "rofi -combi-modi window,drun -show combi -font 'Cascadia Code 14' -icon-theme 'Fluent' -show-icons -dpi 144")
       ,((myModMask .|. shiftMask, xK_p), spawn "rofi -show window -font 'Cascadia Code 14' -icon-theme 'Fluent' -show-icons -dpi 144")
       ,((myModMask, xK_x), spawn "flameshot full -c")
       ,((myModMask .|. shiftMask, xK_x), spawn "flameshot gui")
       ,((myModMask .|. shiftMask, xK_v), spawn "copyq toggle")
       ,((myModMask, xK_backslash), spawn "1password --quick-access")
       ,((mod1Mask .|. controlMask, xK_l), spawn $ if hostname == "archdesktop" then "dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock" else "xlock -mode rain")
       ,((myModMask .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore))
       ,((myModMask, xK_a), spawn "autorandr -c")
       ]
    )

myLogHook dbus = def { ppOutput =
                       \log -> do
                         let delimiter = T.pack " : "
                             (_, tail) = T.breakOn delimiter (T.pack log)
                             layout = drop 20
                               $ T.unpack tail
                         D.send dbus layout }

myLayout = smartBorders $ maximize $ spacingRaw True (Border 0 0 0 0) False (Border 6 6 6 6) True $ layoutHook gnomeConfig ||| desktopLayoutModifiers (ThreeColMid 1 (3/100) (1/2))

myLauncher = "$($HOME/.cabal/bin/yeganesh -x -- -fn 'Monoid-8' -b)"

myModMask = mod4Mask

myStartupHook hostname = do
  startupHook gnomeConfig
  spawn "$HOME/.xmonad/scripts/olybarp.sh"
  -- spawn "$HOME/bin/redmi"
  when (hostname == "archlaptop") $ spawn "$HOME/.xmonad/scripts/autolockx.sh"
  -- spawn "$HOME/.xmonad/scripts/int2t.sh"
  spawn "$HOME/.xmonad/scripts/ay-night-switcherd.sh"
  -- setWMName "LG3D"

myManageHook = composeAll
  [ className =? "mpv" --> doFloat
  , className =? "MComix" --> doFloat
  , className =? "Gnome-mpv" --> doFloat
  , className =? "Gnome-tweaks" --> doFloat
  , className =? "Gnome-control-center" --> doFloat
  , className =? "EasyConnect" --> doFloat
  , className =? "Ulauncher" --> hasBorder False >> doFloat
  , className =? "Gnome-screenshot" --> doFloat
  , className =? "1Password" --> doFloat
  , className =? "Youdao Dict" --> hasBorder False >> doFloat
  , className =? "copyq" --> doCenterFloat
  , title =? "Quick Access — 1Password" --> doFloat
  , title =? "Run Application" --> doFloat
  , title =? "Log Out" --> doFloat
  , title =? "歌词" --> hasBorder False >> doF W.focusDown <+> doF copyToAll
  , isFullscreen --> doFullFloat
  ]

myWorkspaces = miscs 9 ++ ["0"]
  where miscs = map (("" ++) . show) . flip take [1..]

myExtraWorkspaces = [(xK_0, "0")]
