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
    Left = 440
    Top = 20
    Width = 26
    Height = 13
    Caption = 'Text:'
  end
  object lblIconSize: TLabel
    Left = 440
    Top = 52
    Width = 47
    Height = 13
    Caption = 'Icon Size:'
  end
  object lblIconMargin: TLabel
    Left = 440
    Top = 84
    Width = 60
    Height = 13
    Caption = 'Icon Margin:'
  end
  object lblBackColor: TLabel
    Left = 440
    Top = 116
    Width = 60
    Height = 13
    Caption = 'Background:'
  end
  object shpBackColor: TShape
    Left = 512
    Top = 112
    Width = 25
    Height = 22
    OnMouseDown = shpBackColorMouseDown
  end
  object lblForeColor: TLabel
    Left = 440
    Top = 148
    Width = 47
    Height = 13
    Caption = 'QR Color:'
  end
  object shpForeColor: TShape
    Left = 512
    Top = 144
    Width = 25
    Height = 22
    Brush.Color = clBlack
    OnMouseDown = shpForeColorMouseDown
  end
  object btnShowQRImage: TButton
    Left = 560
    Top = 112
    Width = 161
    Height = 57
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
    Top = 48
    Width = 209
    Height = 21
    TabOrder = 2
    Text = '32'
  end
  object edtIconMargin: TEdit
    Left = 512
    Top = 80
    Width = 209
    Height = 21
    TabOrder = 3
    Text = '2'
  end
  object dlgColor: TColorDialog
    Ctl3D = True
  end
end
