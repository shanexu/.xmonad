{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Data.Binary (Word32)
import Network.HostName
import System.Environment (getEnv)
import Text.Read (readMaybe)
import XMonad
  ( Mirror (Mirror),
    Tall (Tall),
    XConfig (borderWidth, focusFollowsMouse, focusedBorderColor, handleEventHook, layoutHook, logHook, manageHook, modMask, normalBorderColor, startupHook, terminal, workspaces),
    appName,
    className,
    composeAll,
    controlMask,
    def,
    doF,
    doFloat,
    doIgnore,
    mod1Mask,
    mod4Mask,
    screenWorkspace,
    sendMessage,
    shiftMask,
    spawn,
    title,
    whenJust,
    windows,
    withFocused,
    xK_0,
    xK_Down,
    xK_Left,
    xK_Right,
    xK_Up,
    xK_b,
    xK_backslash,
    xK_c,
    xK_e,
    xK_g,
    xK_l,
    xK_m,
    xK_p,
    xK_r,
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
    (|||),
  )
import XMonad.Actions.Commands (defaultCommands)
import XMonad.Actions.CopyWindow (copyToAll)
import qualified XMonad.Actions.Navigation2D as N2D
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Config.Xfce (xfceConfig)
import qualified XMonad.DBus as D
import XMonad.Hooks.DynamicLog
  ( PP
      ( ppCurrent,
        ppHidden,
        ppHiddenNoWindows,
        ppLayout,
        ppOutput,
        ppSep,
        ppTitle,
        ppVisible,
        ppVisibleNoWindows,
        ppWsSep
      ),
    dynamicLogWithPP,
    shorten,
    wrap,
  )
import XMonad.Hooks.EwmhDesktops
  ( addEwmhWorkspaceSort,
    ewmhFullscreen,
  )
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat,
    doFullFloat,
    isFullscreen,
  )
import XMonad.Hooks.ServerMode (serverModeEventHook')
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.Maximize (maximize, maximizeRestore)
import XMonad.Layout.MultiToggle
  ( EOT (EOT),
    Toggle (Toggle),
    mkToggle,
    (??),
  )
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, NOBORDERS))
import XMonad.Layout.NoBorders (hasBorder, smartBorders)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.Tabbed
  ( shrinkText,
    tabbedAlways,
  )
import XMonad.Layout.Decoration
  ( activeBorderColor,
    activeColor,
    activeTextColor,
    decoHeight,
    fontName,
    inactiveBorderColor,
    inactiveColor,
    inactiveTextColor,
  )
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    customFloating,
    namedScratchpadAction,
    namedScratchpadManageHook,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.WorkspaceCompare (filterOutWs)
import XMonad.Hooks.Place (placeHook, inBounds, underMouse)
import XMonad.Util.SpawnOnce (spawnOnce)

main :: IO ()
main = do
  desktopSession <- getEnv "DESKTOP_SESSION"
  hostname <- getHostName
  dbus <- D.connect
  _ <- D.requestAccess dbus
  xmonad
    $ N2D.navigation2D
      def
      (xK_Up, xK_Left, xK_Down, xK_Right)
      [ (mod4Mask, N2D.windowGo),
        (mod4Mask .|. shiftMask, N2D.windowSwap)
      ]
      False
    $ ewmhFullscreen . addEwmhWorkspaceSort (pure (filterOutWs [scratchpadWorkspaceTag]))
    $ (myDesktopConfig desktopSession)
      { modMask = myModMask,
        logHook = dynamicLogWithPP (polybarLogHook dbus) <+> logHook (myDesktopConfig desktopSession),
        -- terminal = "tabbed -n tabbed-alacritty -c alacritty --embed",
        terminal = "kitty",
        workspaces = myWorkspaces,
        borderWidth = myBorderWidth hostname,
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
                                ((myModMask, xK_p), spawn "rofi -combi-modi window,drun,run -show combi -show-icons -dpi 144"),
                                ((myModMask, xK_c), spawn "rofi -show calc -modi calc -no-show-match -no-sort -dpi 144"),
                                ((myModMask .|. shiftMask, xK_p), spawn "rofi -show window -show-icons -dpi 144"),
                                ((myModMask, xK_x), spawn "QT_AUTO_SCREEN_SCALE_FACTOR=0 flameshot full -c"),
                                ((myModMask .|. shiftMask, xK_x), spawn "QT_AUTO_SCREEN_SCALE_FACTOR=0 flameshot gui"),
                                ((myModMask .|. shiftMask, xK_v), spawn "copyq toggle"),
                                ((myModMask, xK_backslash), spawn "1password --quick-access"),
                                ((mod1Mask .|. controlMask, xK_l), spawn $ if desktopSession == "xfce" then "xflock4" else "dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock"),
                                ((myModMask, xK_m), sendMessage $ Toggle FULL),
                                ((myModMask .|. shiftMask, xK_m), withFocused (sendMessage . maximizeRestore)),
                                ((myModMask, xK_z), spawn "autorandr -c"),
                                ((myModMask .|. controlMask, xK_t), namedScratchpadAction scratchpads "dropDownTerminal"),
                                ((myModMask .|. controlMask, xK_c), namedScratchpadAction scratchpads "chatbox"),
                                ((myModMask, xK_b), spawn "$HOME/.config/xmonad/scripts/bars.sh")
                              ]
                       )

polybarLogHook dbus =
  def
    { ppCurrent = wrap "%{u#F0C674}%{+u}%{B#f6373B41} " " %{B-}%{-u}",
      ppVisible = hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":}%{B#f6373B41} ") " %{B-}%{A}" name,
      ppLayout = wrap "%{A1:xmonadctl 27:}%{F#fd4d5e}%{T6}\xe800%{T-}%{F-} " "%{A}" . (\x -> if x == "" then "Toggle Full" else x) . drop 17,
      ppHidden = hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":} ") " %{A}" name,
      ppHiddenNoWindows = hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":}%{F#707880} ") " %{F-}%{A}" name,
      ppVisibleNoWindows = Just $ hideNsp $ \name -> wrap ("%{A1:xmonadctl " ++ nameToCmdNo name ++ ":}%{B#f6373B41}%{F#707880} ") " %{F-}%{B-}%{A}" name,
      ppWsSep = "",
      ppSep = "%{F#707880} | %{F-}",
      ppTitle = wrap "%{T3}\xeb7f%{T-} " "" . shorten 50,
      ppOutput = D.send dbus
    }
  where
    nameToCmdNo name =
      let x =
            ( case readMaybe name of
                Just n -> (n - 1) `mod` 10
                Nothing -> 0
            ) ::
              Int
       in show (42 + x)
    hideNsp mapper name = if name == "NSP" then "" else mapper name

myLayout _ = smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ maximize $ borderResize $ smartSpacing 0 $ desktopLayoutModifiers (Tall 1 (1 / 100) (1 / 2) ||| Mirror (Tall 1 (1 / 100) (1 / 2)) ||| spiral (6 / 7) ||| ThreeColMid 1 (1 / 100) (1 / 2) ||| tabbedAlways shrinkText myTabConfig)

myTabConfig =
  def
    { inactiveBorderColor = "#202030",
      activeBorderColor = "#a0a0d0",
      inactiveColor = "#000000",
      activeColor = "#000000",
      inactiveTextColor = "#607070",
      activeTextColor = "#a0d0d0",
      decoHeight = 42,
      fontName = "xft:Monospace-10,LXGW Neo XiHei Screen Full:size=10,Noto Sans CJK KR:size=10"
    }

-- myLauncher = "$($HOME/.cabal/bin/yeganesh -x -- -fn 'Monoid-8' -b)"

myModMask = mod4Mask

myStartupHook _ desktopSession = do
  startupHook (myDesktopConfig desktopSession)
  -- setWMName "LG3D"
  spawnOnce "$HOME/.config/xmonad/scripts/bars.sh"

scratchpads =
  [ -- NS "dropDownTerminal" "tabbed -c -n Drop-Down-Terminal alacritty -o window.opacity=0.80 --embed" (appName =? "Drop-Down-Terminal") (customFloating $ W.RationalRect (1 / 8) (0 / 6) (3 / 4) (2 / 3)),
    NS "dropDownTerminal" "kitty --name Drop-Down-Terminal -o background_opacity=0.80" (appName =? "Drop-Down-Terminal") (customFloating $ W.RationalRect (1 / 8) (0 / 6) (3 / 4) (2 / 3)),
    NS "chatbox" "Chatbox" (appName =? "xyz.chatboxapp.app") (customFloating $ W.RationalRect (1 / 8) (0 / 6) (3 / 4) (2 / 3))
  ]

myManageHook =
  namedScratchpadManageHook scratchpads
    <+> composeAll
      [ -- className =? "mpv" --> doFloat,
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
        title =? "" <&&> className =? "Wine" --> doIgnore, -- wine微信
        title =? "EmojiFloatWnd" --> doFloat, -- 腾讯会议
        className =? "Xfce4-notifyd" --> doIgnore,
        className =? "Wrapper-2.0" --> doFloat,
        className =? "ToDesk" --> doFloat,
        title =? "com.alibabainc.dingtalk" <&&> className =? "com.alibabainc.dingtalk" --> doFloat >> placeHook (inBounds (underMouse (0, 0)))
      ]

myWorkspaces = miscs 9 ++ ["0"]
  where
    miscs = map (("" ++) . show :: Int -> String) . flip take [1 ..]

myExtraWorkspaces = [(xK_0, "0")]

myDesktopConfig desktopSession =
  if desktopSession == "xfce" then xfceConfig else gnomeConfig

myBorderWidth :: HostName -> Word32
myBorderWidth "archdesktop" = 6
myBorderWidth "archlaptop" = 6
myBorderWidth _ = 4
