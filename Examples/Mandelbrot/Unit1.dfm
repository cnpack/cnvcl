object FormMandelbrot: TFormMandelbrot
  Left = 52
  Top = 47
  Width = 1308
  Height = 864
  Caption = 'Mandelbrot Set Picture'
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
  object lblMark: TLabel
    Left = 868
    Top = 39
    Width = 3
    Height = 13
  end
  object grpInfo: TGroupBox
    Left = 904
    Top = 24
    Width = 369
    Height = 353
    Caption = 'Information'
    TabOrder = 0
    object lblDigits: TLabel
      Left = 144
      Top = 304
      Width = 29
      Height = 13
      Caption = 'Digits:'
    end
    object lblPoint: TLabel
      Left = 40
      Top = 128
      Width = 34
      Height = 13
      Caption = 'Center:'
    end
    object edtMaxY: TEdit
      Left = 72
      Top = 24
      Width = 233
      Height = 21
      TabOrder = 0
    end
    object edtMinY: TEdit
      Left = 80
      Top = 224
      Width = 233
      Height = 21
      TabOrder = 1
    end
    object edtMinX: TEdit
      Left = 8
      Top = 88
      Width = 233
      Height = 21
      TabOrder = 2
    end
    object edtMaxX: TEdit
      Left = 128
      Top = 160
      Width = 233
      Height = 21
      TabOrder = 3
    end
  end
end
