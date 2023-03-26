object FormEventHook: TFormEventHook
  Left = 216
  Top = 149
  Width = 309
  Height = 343
  Caption = 'Test Event Hook'
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
  object Button1: TButton
    Left = 88
    Top = 72
    Width = 129
    Height = 25
    Caption = 'Event Hooked'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 192
    Width = 129
    Height = 25
    Caption = 'Show Hooked Status'
    TabOrder = 1
    OnClick = Button2Click
  end
end
