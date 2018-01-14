object IntfHookForm: TIntfHookForm
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'Test Interface Method Hook'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnHook: TButton
    Left = 40
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Hook'
    TabOrder = 0
    OnClick = btnHookClick
  end
  object btnUnhook: TButton
    Left = 176
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Unhook'
    TabOrder = 1
    OnClick = btnUnhookClick
  end
  object btnCall1: TButton
    Left = 312
    Top = 40
    Width = 97
    Height = 25
    Caption = 'Call Intf Method 1'
    TabOrder = 2
    OnClick = btnCall1Click
  end
  object btnCall2: TButton
    Left = 448
    Top = 40
    Width = 97
    Height = 25
    Caption = 'Call Intf Method 2'
    TabOrder = 3
    OnClick = btnCall2Click
  end
end
