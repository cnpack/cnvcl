object FrmMain: TFrmMain
  Left = 143
  Top = 181
  Width = 744
  Height = 531
  Caption = 'CnLEDText Sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object RzPanel1: TPanel
    Left = 0
    Top = 0
    Width = 736
    Height = 157
    Align = alTop
    TabOrder = 0
    object RzLabel4: TLabel
      Left = 11
      Top = 12
      Width = 28
      Height = 13
      Caption = '字体:'
    end
    object RzLabel5: TLabel
      Left = 183
      Top = 12
      Width = 28
      Height = 13
      Caption = '字号:'
    end
    object cbbFont: TComboBox
      Left = 45
      Top = 7
      Width = 125
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbFontChange
    end
    object seHeight: TSpinEdit
      Left = 217
      Top = 8
      Width = 47
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = seHeightChange
    end
    object btn3: TButton
      Left = 306
      Top = 113
      Width = 148
      Height = 33
      Caption = '输出自定义字库'
      TabOrder = 2
      OnClick = btn3Click
    end
    object btn2: TButton
      Left = 306
      Top = 35
      Width = 146
      Height = 33
      Caption = '生成一二级汉字库'
      TabOrder = 3
      OnClick = btn2Click
    end
    object pnl1: TPanel
      Left = 472
      Top = 40
      Width = 100
      Height = 100
      TabOrder = 4
    end
    object btn1: TButton
      Left = 306
      Top = 74
      Width = 148
      Height = 33
      Caption = '生成完整区位码表字库'
      TabOrder = 5
      OnClick = btn1Click
    end
    object mmo1: TMemo
      Left = 9
      Top = 35
      Width = 273
      Height = 111
      Lines.Strings = (
        '数码显示LEDCC')
      ScrollBars = ssVertical
      TabOrder = 6
      OnChange = mmo1Change
    end
  end
  object pnlLED: TPanel
    Left = 0
    Top = 297
    Width = 736
    Height = 188
    Align = alClient
    TabOrder = 1
    object spl1: TSplitter
      Left = 289
      Top = 1
      Width = 3
      Height = 186
      Cursor = crHSplit
    end
    object atv1: TCnAOTreeView
      Left = 1
      Top = 1
      Width = 288
      Height = 186
      Align = alLeft
      Indent = 19
      OnChange = atv1Change
      TabOrder = 0
    end
  end
  object RzStatusBar1: TStatusBar
    Left = 0
    Top = 485
    Width = 736
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object RzSizePanel2: TPanel
    Left = 0
    Top = 157
    Width = 736
    Height = 140
    Align = alTop
    TabOrder = 3
    object mmoInfo: TMemo
      Left = 1
      Top = 1
      Width = 734
      Height = 138
      Align = alClient
      Font.Charset = GB2312_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = '新宋体'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
