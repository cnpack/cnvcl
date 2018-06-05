object FormNative: TFormNative
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 
    'Test some Native Declarations and Methods - Run on D567 and can ' +
    'be verified in BDS.'
  ClientHeight = 399
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bvl1: TBevel
    Left = 208
    Top = 8
    Width = 17
    Height = 41
    Shape = bsLeftLine
  end
  object btnUInt64Div: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'UInt64 Div'
    TabOrder = 0
    OnClick = btnUInt64DivClick
  end
  object btnUInt64Mod: TButton
    Left = 112
    Top = 16
    Width = 75
    Height = 25
    Caption = 'UInt64 Mod'
    TabOrder = 1
    OnClick = btnUInt64ModClick
  end
  object mmoRes: TMemo
    Left = 16
    Top = 64
    Width = 577
    Height = 316
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
end
