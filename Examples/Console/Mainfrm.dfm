object Form1: TForm1
  Left = 355
  Top = 318
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 88
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'OpenConsole'
    TabOrder = 0
    OnClick = Button1Click
  end
  object btnOff: TButton
    Left = 256
    Top = 24
    Width = 75
    Height = 25
    Caption = 'CloseConsole'
    TabOrder = 1
    OnClick = btnOffClick
  end
  object btnrst: TButton
    Left = 136
    Top = 24
    Width = 75
    Height = 25
    Caption = 'ResetConsole'
    TabOrder = 2
    OnClick = btnrstClick
  end
  object CnConsole1: TCnConsole
    Enabled = False
    Left = 216
    Top = 8
  end
end
