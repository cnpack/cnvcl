object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Hint = '窗体的 Hint'
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object lbl1: TLabel
    Left = 32
    Top = 24
    Width = 36
    Height = 12
    Hint = 'Label 的 Hint'
    Caption = 'label1'
    ParentShowHint = False
    ShowHint = True
  end
  object bvl1: TBevel
    Left = 88
    Top = 256
    Width = 569
    Height = 17
    Shape = bsBottomLine
  end
  object lbl2: TLabel
    Left = 512
    Top = 312
    Width = 132
    Height = 12
    Caption = '设置气泡提示的 Title：'
  end
  object lbl3: TLabel
    Left = 512
    Top = 384
    Width = 120
    Height = 12
    Caption = '设置气泡提示的图标：'
  end
  object btn1: TSpeedButton
    Left = 616
    Top = 408
    Width = 23
    Height = 22
    Caption = '...'
    Flat = True
    OnClick = btn1Click
  end
  object lbl4: TLabel
    Left = 32
    Top = 264
    Width = 84
    Height = 12
    Hint = 'Label 的 Hint'
    Caption = '全局提示设置：'
    ParentShowHint = False
    ShowHint = True
  end
  object Edit1: TEdit
    Left = 96
    Top = 24
    Width = 121
    Height = 20
    Hint = 'Edit1 的 Hint'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 32
    Top = 80
    Width = 75
    Height = 25
    Hint = 'Button 的 Hint'#13#10'Button 的分行 Hint'
    Caption = 'Button1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 32
    Top = 144
    Width = 185
    Height = 89
    Hint = 'Memo 的 Hint'
    Lines.Strings = (
      'Memo1')
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 144
    Top = 80
    Width = 97
    Height = 17
    Hint = 'CheckBox 的 Hint'
    Caption = 'CheckBox1'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
  end
  object RadioGroup1: TRadioGroup
    Left = 32
    Top = 304
    Width = 209
    Height = 129
    Caption = '提示框角位置（Hint Position）'
    Items.Strings = (
      '右下'
      '左下'
      '右上'
      '左上')
    TabOrder = 4
    OnClick = RadioGroup1Click
  end
  object RadioGroup2: TRadioGroup
    Left = 272
    Top = 304
    Width = 209
    Height = 129
    Caption = '提示框风格'
    Items.Strings = (
      '普通提示'
      '气泡提示'
      '自动（根据有无Title处理）')
    TabOrder = 5
    OnClick = RadioGroup2Click
  end
  object Edit2: TEdit
    Left = 512
    Top = 336
    Width = 121
    Height = 20
    TabOrder = 6
    Text = 'Edit2'
    OnChange = Edit2Change
  end
  object edt1: TEdit
    Left = 512
    Top = 408
    Width = 97
    Height = 20
    TabOrder = 7
    OnChange = Edit2Change
    OnKeyPress = edt1KeyPress
  end
  object pnl1: TPanel
    Left = 256
    Top = 24
    Width = 401
    Height = 209
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = '点击此 Panel 弹出手工提示，不受全局设置的影响'
    TabOrder = 8
    OnClick = FormClick
  end
  object dlgOpen1: TOpenDialog
    Left = 576
    Top = 360
  end
  object C: TCnHint
    BackColor = clInfoBk
    BorderColor = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HintStyle = hsNormal
    Left = 104
    Top = 176
  end
  object Cw: TCnHintWindow
    HintPosition = hpUpLeft
    Glyph.Data = {
      5A010000424D5A01000000000000760000002800000012000000130000000100
      040000000000E400000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDD44DDDDD
      DDDDDD000000DDDD4224DDDDDDDDDD000000DDD422224DDDDDDDDD000000DD42
      222224DDDDDDDD000000D4222A22224DDDDDDD000000D222A2A2224DDDDDDD00
      0000DA2A222A2224DDDDDD000000D4A22222A2224DDDDD0000004222A2222A22
      24DDDD000000222ADA2224A2224DDD000000A2ADDDA2224A2224DD000000DADD
      DDDA2224A2224D000000DDDDDDDDA2224A224D000000DDDDDDDDDA2224A22D00
      0000DDDDDDDDDDA2224ADD000000DDDDDDDDDDDA2224DD000000DDDDDDDDDDDD
      A224DD000000DDDDDDDDDDDDDA22DD000000DDDDDDDDDDDDDDADDD000000}
    Alignment = taLeftJustify
    HintStyle = hsAuto
    Left = 576
    Top = 64
  end
end
