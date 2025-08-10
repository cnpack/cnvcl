object FormFile: TFormFile
  Left = 192
  Top = 112
  BorderStyle = bsDialog
  Caption = 'File Utils Test'
  ClientHeight = 513
  ClientWidth = 706
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblFind: TLabel
    Left = 16
    Top = 24
    Width = 23
    Height = 13
    Caption = 'Find:'
  end
  object lblIn: TLabel
    Left = 128
    Top = 24
    Width = 12
    Height = 13
    Caption = 'In:'
  end
  object edtPattern: TEdit
    Left = 56
    Top = 24
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '*'
  end
  object edtPath: TEdit
    Left = 160
    Top = 24
    Width = 433
    Height = 21
    TabOrder = 1
    Text = 'C:\'
  end
  object btnFind: TButton
    Left = 16
    Top = 64
    Width = 577
    Height = 25
    Caption = 'Find! Find!! Find!!!'
    TabOrder = 2
    OnClick = btnFindClick
  end
  object mmoResult: TMemo
    Left = 16
    Top = 112
    Width = 665
    Height = 377
    TabOrder = 3
  end
  object btnBrowse: TButton
    Left = 608
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 4
    OnClick = btnBrowseClick
  end
  object chkCancel: TCheckBox
    Left = 608
    Top = 72
    Width = 73
    Height = 17
    Caption = 'Cancel'
    TabOrder = 5
  end
  object dlgOpen1: TOpenDialog
    Left = 336
    Top = 240
  end
end
