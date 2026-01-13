object FormQRTest: TFormQRTest
  Left = 296
  Top = 225
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
    Left = 456
    Top = 20
    Width = 26
    Height = 13
    Caption = 'Text:'
  end
  object lblIconSize: TLabel
    Left = 456
    Top = 92
    Width = 44
    Height = 13
    Caption = 'IconSize:'
  end
  object lblIconMargin: TLabel
    Left = 440
    Top = 124
    Width = 60
    Height = 13
    Caption = 'Icon Margin:'
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
  object edtIconSize: TEdit
    Left = 512
    Top = 88
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '32'
  end
  object edtIconMargin: TEdit
    Left = 512
    Top = 120
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '32'
  end
end
