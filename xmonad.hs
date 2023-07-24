import Control.Monad (when)
import DBus.Client qualified as DC
import Data.Text qualified as T
import Data.Text.Encoding.Error (ignore)
import Network.HostName
import System.Directory
import XMonad
  ( XConfig (borderWidth, focusFollowsMouse, focusedBorderColor, handleEventHook, layoutHook, logHook, manageHook, modMask, normalBorderColor, startupHook, terminal, workspaces),
    appName,
    className,
    composeAll,
    controlMask,
    doF,
    doFloat,
    doIgnore,
    ifM,
    mod1Mask,
    mod4Mask,
    resource,
    screenWorkspace,
    sendMessage,
    shiftMask,
    spawn,
    stringProperty,
    title,
    whenJust,
    windows,
    withFocused,
    xK_0,
    xK_Down,
    xK_F2,
    xK_Left,
    xK_Right,
    xK_Up,
    xK_a,
    xK_backslash,
    xK_c,
    xK_e,
    xK_g,
    xK_h,
    xK_j,
    xK_k,
    xK_l,
    xK_m,
    xK_n,
    xK_p,
    xK_r,
    xK_s,
    xK_t,
    xK_v,
    xK_w,
    xK_x,
    xK_z,
    xmonad,
    (-->),
    (.|.),
    (<&&>),
    (<+>),
    (=?),
    (|||), JumpToLayout (JumpToLayout),
  )
import XMonad.Actions.Commands (defaultCommands)
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Actions.CycleWS (Direction1D (Next, Prev))
import XMonad.Actions.Navigation2D
import XMonad.Actions.NoBorders
import XMonad.Config.Desktop
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Config.Xfce (xfceConfig)
import XMonad.DBus qualified as D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat,
    doFullFloat,
    isFullscreen,
  )
import XMonad.Hooks.Place (smart)
import XMonad.Hooks.ServerMode (serverModeEventHook, serverModeEventHook')
import XMonad.Hooks.SetWMName
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed (tabbed)
import XMonad.Layout.ThreeColumns
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes (ThemeInfo (theme))
import XMonad.Util.WorkspaceCompare (filterOutWs)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(FULL, NOBORDERS))
import System.Environment (getEnv)

main :: IO ()
main = do
  desktopSession <- getEnv "DESKTOP_SESSION"
  hostname <- getHostName
  dbus <- D.connect
  D.requestAccess dbus
  xmonad
    $ navigation2D
      def
      (xK_Up, xK_Left, xK_Down, xK_Right)
      [ (mod4Mask, windowGo),
        (mod4Mask .|. shiftMask, windowSwap)
      ]
      False
    $ ewmhFullscreen . addEwmhWorkspaceSort (pure (filterOutWs [scratchpadWorkspaceTag]))
    $ (myDesktopConfig desktopSession)
      { modMask = myModMask,
        logHook = dynamicLogWithPP (polybarLogHook dbus) <+> logHook (myDesktopConfig desktopSession),
        terminal = "tabbed -n tabbed-alacritty -c alacritty --embed",
        workspaces = myWorkspaces,
        borderWidth = 4,
        focusFollowsMouse = True,
        layoutHook = myLayout desktopSession,
        normalBorderColor = "#555555",
        focusedBorderColor = "#f36864",
        handleEventHook =
          serverModeEventHook'
            ( defaultCommands >>= \cmds ->
                return $
                  cmds
                    ++ [("greedyView " ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces]
            )
            <+> handleEventHook (myDesktopConfig desktopSession),
        startupHook = myStartupHook hostname desktopSession,
        manageHook = myManageHook <+> manageHook (myDesktopConfig desktopSession)
      }
      `additionalKeys` ( [((myModMask, key), windows $ W.greedyView ws) | (key, ws) <- myExtraWorkspaces]
                           ++ [((myModMask .|. shiftMask, key), windows $ W.shift ws) | (key, ws) <- myExtraWorkspaces]
                           ++ [ ((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
                                | (key, sc) <- zip [xK_w, xK_e, xK_r] [2, 0, 1], -- was [0..] *** change to match your screen order ***
                                  (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
                              ]
                           ++ [ ((myModMask, xK_g), withFocused toggleBorder),
                                ((myModMask, xK_p), spawn "rofi -combi-modi window,drun,run -show combi -font 'CaskaydiaCove Nerd Font 14' -icon-theme 'Fluent' -show-icons -dpi 144"),
                                ((myModMask .|. shiftMask, xK_p), spawn "rofi -show window -font 'CaskaydiaCove Nerd Font 14' -icon-theme 'Fluent' -show-icons -dpi 144"),
                                ((myModMask, xK_x), spawn "QT_AUTO_SCREEN_SCALE_FACTOR=0 flameshot full -c"),
                                ((myModMask .|. shiftMask, xK_x), spawn "QT_AUTO_SCREEN_SCALE_FACTOR=0 flameshot gui"),
                                ((myModMask .|. shiftMask, xK_v), spawn "copyq toggle"),
                                ((myModMask, xK_backslash), spawn "1password --quick-access"),
                                ((mod1Mask .|. controlMask, xK_l), spawn $ if desktopSession == "xfce" then "xflock4" else "dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock"),
                                ((myModMask, xK_m), sendMessage $ Toggle FULL),
                                ((myModMask .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore)),
                                ((myModMask, xK_z), spawn "autorandr -c"),
                                ((myModMask .|. controlMask, xK_t), namedScratchpadAction scratchpads "dropDownTerminal"),
                                -- bsp
                                ((myModMask .|. mod1Mask, xK_l), sendMessage $ ExpandTowards R),
                                ((myModMask .|. mod1Mask, xK_h), sendMessage $ ExpandTowards L),
                                ((myModMask .|. mod1Mask, xK_j), sendMessage $ ExpandTowards D),
                                ((myModMask .|. mod1Mask, xK_k), sendMessage $ ExpandTowards U),
                                ((myModMask .|. mod1Mask .|. controlMask, xK_l), sendMessage $ ShrinkFrom R),
                                ((myModMask .|. mod1Mask .|. controlMask, xK_h), sendMessage $ ShrinkFrom L),
                                ((myModMask .|. mod1Mask .|. controlMask, xK_j), sendMessage $ ShrinkFrom D),
                                ((myModMask .|. mod1Mask .|. controlMask, xK_k), sendMessage $ ShrinkFrom U),
                                ((myModMask .|. mod1Mask, xK_r), sendMessage Rotate),
                                ((myModMask, xK_s), sendMessage Swap),
                                ((myModMask, xK_n), sendMessage FocusParent),
                                ((myModMask .|. controlMask, xK_n), sendMessage SelectNode),
                                ((myModMask .|. shiftMask, xK_n), sendMessage MoveNode),
                                ((myModMask .|. shiftMask .|. controlMask, xK_j), sendMessage $ SplitShift Prev),
                                ((myModMask .|. shiftMask .|. controlMask, xK_k), sendMessage $ SplitShift Next),
                                ((myModMask, xK_a), sendMessage Balance),
                                ((myModMask .|. mod1Mask, xK_a), sendMessage Equalize)
                              ]
                       )

polybarLogHook dbus =
  def
    { ppCurrent = wrap "%{u#F0C674}%{+u}%{B#f6373B41} " " %{B-}%{-u}",
      ppVisible = hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":}%{B#f6373B41} ") " %{B-}%{A}" name,
      ppLayout = wrap "%{A1:xmonadctl 27:}%{F#fd4d5e}%{T6}\xe800%{T-}%{F-} " "%{A}" . drop 17,
      ppHidden = hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":} ") " %{A}" name,
      ppHiddenNoWindows = hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":}%{F#707880} ") " %{F-}%{A}" name,
      ppVisibleNoWindows = Just $ hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":}%{B#f6373B41}%{F#707880} ") " %{F-}%{B-}%{A}" name,
      ppWsSep = "",
      ppSep = "%{F#707880} | %{F-}",
      ppTitle = wrap "%{T3}\xeb7f%{T-} " "" . shorten 50,
      ppOutput = D.send dbus
    }
  where
    nameToCmdNo name = show (((read name - 1) `mod` 10) + 42 :: Int)
    hideNsp mapper name = if name == "NSP" then "" else mapper name

myLayout desktopSession = smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ maximize $ borderResize $ smartSpacing 0 $ layoutHook (myDesktopConfig desktopSession) ||| desktopLayoutModifiers (ThreeColMid 1 (3 / 100) (1 / 2) ||| emptyBSP)

myLauncher = "$($HOME/.cabal/bin/yeganesh -x -- -fn 'Monoid-8' -b)"

myModMask = mod4Mask

myStartupHook hostname desktopSession = do
  startupHook (myDesktopConfig desktopSession)
  -- spawn "$HOME/.config/xmonad/scripts/ay-night-switcherd.sh"
  -- spawn "$HOME/.config/xmonad/scripts/bars.sh"

scratchpads =
  [ NS "dropDownTerminal" "tabbed -c -n Drop-Down-Terminal alacritty -o window.opacity=0.80 --embed" (appName =? "Drop-Down-Terminal") (customFloating $ W.RationalRect (1 / 8) (0 / 6) (3 / 4) (2 / 3))
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook =
  namedScratchpadManageHook scratchpads
    <+> composeAll
      [ className =? "mpv" --> doFloat,
        className =? "MComix" --> doFloat,
        className =? "Gnome-mpv" --> doFloat,
        className =? "Gnome-tweaks" --> doFloat,
        className =? "Gnome-control-center" --> doFloat,
        className =? "EasyConnect" --> doFloat,
        className =? "Ulauncher" --> hasBorder False >> doFloat,
        className =? "Gnome-screenshot" --> doFloat,
        className =? "1Password" --> doFloat,
        className =? "Youdao Dict" --> hasBorder False >> doFloat,
        className =? "copyq" --> doCenterFloat,
        title =? "Quick Access — 1Password" --> doFloat,
        title =? "Run Application" --> doFloat,
        title =? "Log Out" --> doFloat,
        title =? "歌词" --> hasBorder False >> doF W.focusDown <+> doF copyToAll,
        isFullscreen --> doFullFloat,
        title =? "" <&&> className =? "Wine" --> doIgnore
      ]

myWorkspaces = miscs 9 ++ ["0"]
  where
    miscs = map (("" ++) . show) . flip take [1 ..]

myExtraWorkspaces = [(xK_0, "0")]

myDesktopConfig desktopSession =
  if desktopSession == "xfce" then xfceConfig else gnomeConfig
