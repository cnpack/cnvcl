object FormFuzzy: TFormFuzzy
  Left = 193
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Test String Fuzzy Match'
  ClientHeight = 374
  ClientWidth = 668
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ËÎÌו'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object lblSearch: TLabel
    Left = 16
    Top = 24
    Width = 42
    Height = 12
    Caption = 'Search:'
  end
  object chkCase: TCheckBox
    Left = 432
    Top = 24
    Width = 113
    Height = 17
    Caption = 'Case Sensitive'
    TabOrder = 1
    OnClick = edtSearchChange
  end
  object edtSearch: TEdit
    Left = 64
    Top = 22
    Width = 353
    Height = 20
    TabOrder = 0
    OnChange = edtSearchChange
  end
  object mmoResult: TMemo
    Left = 64
    Top = 72
    Width = 569
    Height = 273
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object chkScore: TCheckBox
    Left = 560
    Top = 24
    Width = 97
    Height = 17
    Caption = 'Use Score'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = edtSearchChange
  end
end
