object Form1: TForm1
  Left = 119
  Top = 27
  Width = 347
  Height = 326
  Caption = 'Twain Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 323
    Height = 250
  end
  object Button1: TButton
    Left = 256
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Acquire'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CnTwain1: TCnTwain
    AutoFeed = False
    TransferType = doNativeTransfer
    OnCaptrue = CnTwain1Captrue
    Left = 120
    Top = 176
  end
end
