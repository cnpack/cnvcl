object MainForm: TMainForm
  Left = 484
  Top = 191
  Width = 310
  Height = 316
  Anchors = []
  Caption = '主窗体'
  Color = clGray
  DefaultMonitor = dmMainForm
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  FormStyle = fsMDIForm
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 302
    Height = 24
    Anchors = []
    AutoSize = True
    ButtonHeight = 20
    ButtonWidth = 67
    Caption = 'ToolBar1'
    Color = clBtnFace
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    Flat = True
    ParentColor = False
    ShowCaptions = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'Delphi风格'
      ImageIndex = 0
      OnClick = DelphiStyleClick
    end
    object ToolButton2: TToolButton
      Left = 71
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'VC++风格'
      ImageIndex = 1
      OnClick = VCStyleClick
    end
    object ToolButton3: TToolButton
      Left = 130
      Top = 0
      AllowAllUp = True
      AutoSize = True
      Caption = 'VID风格'
      ImageIndex = 2
      OnClick = VIDStyleClick
    end
    object ToolButton4: TToolButton
      Left = 183
      Top = 0
      Caption = 'VS.NET风格'
      ImageIndex = 3
      OnClick = DockForm4Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 251
    Width = 302
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 24
    Width = 302
    Height = 227
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '宋体'
    Font.Style = []
    Lines.Strings = (
      
        '欢迎大家使用DockPresident控件。这个Demo程序用来演示控件的使用方' +
        '法。'
      
        '其中在程序的工具栏上面有四个按钮：'#39'Delphi风格'#39','#39'VC++风格'#39','#39'VID风' +
        '格'#39'和'#39'VS.NET风格'#39'。'
      #39'Delphi风格'#39'用来创建Delphi停靠风格的窗体。'
      #39'VC++风格'#39'用来创建Visual C++停靠风格的窗体。'
      #39'VID风格'#39'用来创建Visual InterDev停靠风格的窗体。'
      #39'VS.NET风格'#39'用来创建Visual Studio.NET停靠风格的窗体。'
      
        '如果用户希望看到Delphi的停靠风格，请在设计期的时候把主窗体上面的' +
        #39'CnDockServer1'#39'控件的DockStyle属性设置成CnDelphiDockStyle1;然后' +
        '在运行期点击'#39'Delphi风格'#39'按钮创建停靠窗体,把这个停靠窗体拖动到主' +
        '窗体附近就可以实现Delphi的停靠风格。'
      
        '如果用户希望看到Visual C++的停靠风格，请在设计期的时候把主窗体上' +
        '面的'#39'CnDockServer1'#39'控件的DockStyle属性设置成CnVCDockStyle1;然后' +
        '在运行期点击'#39'VC++风格'#39'按钮创建停窗体,把这个停靠窗体拖动到主窗体' +
        '附近就可以实现Visual C++的停靠风格。'
      
        '如果用户希望看到Visual InterDev的停靠风格，请在设计期的时候把主' +
        '窗体上面的'#39'CnDockServer1'#39'控件的DockStyle属性设置成CnVIDDockStyle' +
        '1;然后在运行期点击'#39'VID风格'#39'按钮创建停靠窗体,把这个停靠窗体拖动到' +
        '主窗体附近就可以实现Visual InterDev的停靠风格。'
      
        '如果用户希望看到Visual Studio.NET的停靠风格，请在设计期的时候把' +
        '主窗体上面的'#39'CnDockServer1'#39'控件的DockStyle属性设置成CnVSNETDockS' +
        'tyle1;然后在运行期点击'#39'VSNET风格'#39'按钮创建停靠窗体,把这个停靠窗体' +
        '拖动到主窗体附近就可以实现Visual Studio.NET的停靠风格。')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 112
    Top = 48
    object DockForm_Menu: TMenuItem
      Caption = '停靠窗体'
      object DelphiStyle: TMenuItem
        Caption = 'Delphi风格'
        OnClick = DelphiStyleClick
      end
      object VCStyle: TMenuItem
        Caption = 'VC++风格'
        OnClick = VCStyleClick
      end
      object VIDStyle: TMenuItem
        Caption = 'VID风格'
        OnClick = VIDStyleClick
      end
      object VSNETStyle: TMenuItem
        Caption = 'VSNET风格'
      end
    end
    object ShowWindow_Menu: TMenuItem
      Caption = '显示窗体'
    end
    object DockInfo_Menu: TMenuItem
      Caption = '停靠信息'
      object SaveToFile: TMenuItem
        Caption = '存储到文件'
        OnClick = SaveToFileClick
      end
      object LoadFromFile: TMenuItem
        Caption = '从文件还原'
        OnClick = LoadFromFileClick
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object SaveToReg: TMenuItem
        Caption = '存储到注册表'
        OnClick = SaveToRegClick
      end
      object LoadFromReg: TMenuItem
        Caption = '从注册表还原'
        OnClick = LoadFromRegClick
      end
    end
    object DockStyle_Menu: TMenuItem
      Caption = '停靠风格'
      Visible = False
      object Default: TMenuItem
        Caption = '默认'
        OnClick = DefaultClick
      end
      object DelphiDockStyle: TMenuItem
        Caption = 'Delphi'
        OnClick = DelphiDockStyleClick
      end
      object VCDockStyle: TMenuItem
        Caption = 'Visual C++'
        OnClick = VCDockStyleClick
      end
      object VIDDockStyle: TMenuItem
        Caption = 'Visual InterDev'
        OnClick = VIDDockStyleClick
      end
    end
    object DockOption_Menu: TMenuItem
      Caption = '停靠选项'
      object TopDocked: TMenuItem
        Caption = '上边可停靠'
        OnClick = TopDockedClick
      end
      object BottomDocked: TMenuItem
        Caption = '下边可停靠'
        OnClick = BottomDockedClick
      end
      object LeftDocked: TMenuItem
        Caption = '左边可停靠'
        OnClick = LeftDockedClick
      end
      object RightDocked: TMenuItem
        Caption = '右边可停靠'
        OnClick = RightDockedClick
      end
      object N31: TMenuItem
        Caption = '-'
      end
      object AllDocked: TMenuItem
        Caption = '全局可停靠'
        OnClick = AllDockedClick
      end
    end
    object Set_Menu: TMenuItem
      Caption = '设置'
      Visible = False
      object N10: TMenuItem
        Caption = '显示弹出菜单'
        OnClick = N10Click
      end
      object N11: TMenuItem
        Caption = '是否高亮显示'
        OnClick = N11Click
      end
      object N12: TMenuItem
        Caption = '分叶服务器的风格'
        object N13: TMenuItem
          Tag = 1
          Caption = 'bsDialog'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N14: TMenuItem
          Tag = 2
          Caption = 'bsNone'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N15: TMenuItem
          Tag = 3
          Caption = 'bsSingle'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N16: TMenuItem
          Tag = 4
          Caption = 'bsSizeable'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object N17: TMenuItem
          Tag = 5
          Caption = 'bsSizeToolWin'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
        object bsToolWindow1: TMenuItem
          Tag = 6
          Caption = 'bsToolWindow'
          RadioItem = True
          OnClick = bsToolWindow1Click
        end
      end
      object N18: TMenuItem
        Caption = '平铺服务器的风格'
        object bsDialog1: TMenuItem
          Tag = 1
          Caption = 'bsDialog'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsNone1: TMenuItem
          Tag = 2
          Caption = 'bsNone'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsSingle1: TMenuItem
          Tag = 3
          Caption = 'bsSingle'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsSizeable1: TMenuItem
          Tag = 4
          Caption = 'bsSizeable'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsSizeToolWin1: TMenuItem
          Tag = 5
          Caption = 'bsSizeToolWin'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
        object bsToolWindow2: TMenuItem
          Tag = 6
          Caption = 'bsToolWindow'
          RadioItem = True
          OnClick = bsToolWindow2Click
        end
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 192
    Top = 48
    object N5: TMenuItem
      Tag = 1
      Caption = '上边'
      OnClick = N5Click
    end
    object N6: TMenuItem
      Tag = 2
      Caption = '左边'
      OnClick = N5Click
    end
    object N7: TMenuItem
      Tag = 3
      Caption = '下边'
      OnClick = N5Click
    end
    object N8: TMenuItem
      Tag = 4
      Caption = '右边'
      OnClick = N5Click
    end
  end
  object CnDockServer1: TCnDockServer
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    LeftSplitterStyle.Size = 4
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    TopSplitterStyle.Size = 4
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    RightSplitterStyle.Size = 4
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    BottomSplitterStyle.Size = 4
    DockStyle = CnVSNETDockStyle1
    Left = 32
    Top = 48
  end
  object CnDelphiDockStyle1: TCnDelphiDockStyle
    ConjoinServerOption.GrabbersSize = 12
    ConjoinServerOption.SplitterWidth = 4
    Left = 32
    Top = 112
  end
  object CnVCDockStyle1: TCnVCDockStyle
    ConjoinServerOption.GrabbersSize = 15
    ConjoinServerOption.SplitterWidth = 4
    Left = 112
    Top = 112
  end
  object CnVIDDockStyle1: TCnVIDDockStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = [fsBold]
    ConjoinServerOption.InactiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.InactiveFont.Color = 13160660
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = [fsBold]
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = 15780518
    ConjoinServerOption.InactiveTitleStartColor = clGray
    ConjoinServerOption.InactiveTitleEndColor = clSilver
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clBtnShadow
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = clWhite
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = False
    Left = 192
    Top = 112
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 112
    Top = 176
    object ClientTopDocked: TMenuItem
      Caption = '上边可停靠'
      Checked = True
      OnClick = ClientTopDockedClick
    end
    object ClientBottomDocked: TMenuItem
      Caption = '下边可停靠'
      Checked = True
      OnClick = ClientBottomDockedClick
    end
    object ClientLeftDocked: TMenuItem
      Caption = '左边可停靠'
      Checked = True
      OnClick = ClientLeftDockedClick
    end
    object ClientRightDocked: TMenuItem
      Caption = '右边可停靠'
      Checked = True
      OnClick = ClientRightDockedClick
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object ClientEachOtherDocked: TMenuItem
      Caption = '相互可停靠'
      Checked = True
      OnClick = ClientEachOtherDockedClick
    end
    object ClientAllDocked: TMenuItem
      Caption = '全局可停靠'
      Checked = True
      OnClick = ClientAllDockedClick
    end
    object N21: TMenuItem
      Caption = '-'
    end
    object ClientDockorFloat: TMenuItem
      Caption = '停靠'
      OnClick = ClientDockorFloatClick
    end
    object ClientHide: TMenuItem
      Caption = '隐藏'
      OnClick = ClientHideClick
    end
  end
  object CnVSNETDockStyle1: TCnVSNETDockStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = []
    ConjoinServerOption.InactiveFont.Charset = GB2312_CHARSET
    ConjoinServerOption.InactiveFont.Color = clBlack
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = []
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = 6956042
    ConjoinServerOption.InactiveTitleStartColor = clBtnFace
    ConjoinServerOption.InactiveTitleEndColor = clBtnFace
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = clInfoBk
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = 5395794
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = True
    ChannelOption.ActivePaneSize = 150
    ChannelOption.ShowImage = True
    Left = 32
    Top = 176
  end
end
