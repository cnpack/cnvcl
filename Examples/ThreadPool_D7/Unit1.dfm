object frmTest: TfrmTest
  Left = 460
  Top = 229
  Width = 296
  Height = 427
  Caption = #32447#31243#27744#27979#35797
  Color = clBtnFace
  Constraints.MinHeight = 427
  Constraints.MinWidth = 296
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShortCut = FormShortCut
  DesignSize = (
    288
    400)
  PixelsPerInch = 96
  TextHeight = 12
  object lbl1: TLabel
    Left = 188
    Top = 10
    Width = 48
    Height = 12
    Anchors = [akTop, akRight]
    Caption = #30417#21548#31471#21475
  end
  object lbl2: TLabel
    Left = 148
    Top = 182
    Width = 12
    Height = 12
    Anchors = [akTop, akRight]
    Caption = 'IP'
  end
  object lbl3: TLabel
    Left = 84
    Top = 182
    Width = 24
    Height = 12
    Anchors = [akTop, akRight]
    Caption = #38388#38548
  end
  object btn3: TSpeedButton
    Left = 4
    Top = 4
    Width = 75
    Height = 25
    Caption = #20351#29992#35828#26126
    OnClick = btn3Click
  end
  object btn1: TButton
    Left = 84
    Top = 3
    Width = 101
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = #24320#22987#30417#21548
    TabOrder = 0
    OnClick = btn1Click
  end
  object edt1: TEdit
    Left = 240
    Top = 6
    Width = 45
    Height = 20
    Anchors = [akTop, akRight]
    TabOrder = 1
    Text = '5999'
    OnChange = edt1Change
  end
  object mmo1: TMemo
    Left = 4
    Top = 32
    Width = 281
    Height = 137
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'mmo1')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btn2: TButton
    Left = 4
    Top = 176
    Width = 77
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = #24320#22987#21457#36865
    TabOrder = 3
    OnClick = btn2Click
  end
  object edt2: TComboBox
    Left = 164
    Top = 178
    Width = 121
    Height = 20
    Anchors = [akTop, akRight]
    ItemHeight = 12
    ItemIndex = 0
    TabOrder = 5
    Text = '200.200.200.215:5999'
    Items.Strings = (
      '200.200.200.215:5999'
      '127.0.0.1:5999')
  end
  object rg1: TRadioGroup
    Left = 4
    Top = 204
    Width = 281
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    Caption = ' '#21457#36865#36873#39033' '
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      #21487#25340#21512
      #19981#21487#25340#21512)
    TabOrder = 6
  end
  object edt3: TEdit
    Left = 4
    Top = 248
    Width = 281
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object mmo2: TMemo
    Left = 3
    Top = 272
    Width = 281
    Height = 117
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmo1')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object edt4: TEdit
    Left = 111
    Top = 178
    Width = 34
    Height = 20
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '100'
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 0
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnExecute = IdTCPServer1Execute
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 128
    Top = 4
  end
  object tmr1: TTimer
    OnTimer = tmr1Timer
    Left = 136
    Top = 200
  end
end
