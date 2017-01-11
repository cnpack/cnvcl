object FormFuzzy: TFormFuzzy
  Left = 193
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Test String Fuzzy Match'
  ClientHeight = 374
  ClientWidth = 668
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object lblSearch: TLabel
    Left = 16
    Top = 24
    Width = 41
    Height = 14
    Caption = 'Search:'
  end
  object pbString: TPaintBox
    Left = 64
    Top = 56
    Width = 569
    Height = 25
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    OnPaint = pbStringPaint
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
    Height = 22
    TabOrder = 0
    OnChange = edtSearchChange
  end
  object mmoResult: TMemo
    Left = 64
    Top = 96
    Width = 569
    Height = 249
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
