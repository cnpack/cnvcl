object FormQRTest: TFormQRTest
  Left = 296
  Top = 191
  Width = 747
  Height = 468
  Caption = 'QR Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblText: TLabel
    Left = 472
    Top = 20
    Width = 26
    Height = 13
    Caption = 'Text:'
  end
  object btnShowQRImage: TButton
    Left = 512
    Top = 48
    Width = 209
    Height = 25
    Caption = 'Show QR Code Image'
    TabOrder = 0
    OnClick = btnShowQRImageClick
  end
  object edtQRText: TEdit
    Left = 512
    Top = 16
    Width = 209
    Height = 21
    TabOrder = 1
    Text = 'CnPack Sample QR Code.'
  end
end
