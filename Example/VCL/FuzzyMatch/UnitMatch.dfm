object FormFuzzy: TFormFuzzy
  Left = 193
  Top = 109
  BorderStyle = bsDialog
  Caption = 'Test String Fuzzy Match'
  ClientHeight = 543
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object lblSearch: TLabel
    Left = 16
    Top = 24
    Width = 36
    Height = 12
    Caption = 'Fuzzy:'
  end
  object pbString: TPaintBox
    Left = 64
    Top = 96
    Width = 569
    Height = 41
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    OnPaint = pbStringPaint
  end
  object bvl: TBevel
    Left = 64
    Top = 424
    Width = 569
    Height = 17
    Shape = bsTopLine
  end
  object lblSearchKMP: TLabel
    Left = 16
    Top = 440
    Width = 66
    Height = 12
    Caption = 'KMP Search:'
  end
  object lblKMPIn: TLabel
    Left = 256
    Top = 440
    Width = 12
    Height = 12
    Caption = 'in'
  end
  object lblSep: TLabel
    Left = 16
    Top = 56
    Width = 24
    Height = 12
    Caption = 'Sep:'
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
    Top = 152
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
  object edtKMPPattern: TEdit
    Left = 88
    Top = 440
    Width = 153
    Height = 20
    TabOrder = 4
    Text = 'abababca'
  end
  object edtKMPText: TEdit
    Left = 280
    Top = 440
    Width = 217
    Height = 20
    TabOrder = 5
    Text = 'ababababca'
  end
  object btnKMPSearch: TButton
    Left = 520
    Top = 440
    Width = 113
    Height = 25
    Caption = 'KMP Search'
    TabOrder = 6
    OnClick = btnKMPSearchClick
  end
  object edtSepSearch: TEdit
    Left = 64
    Top = 54
    Width = 353
    Height = 20
    TabOrder = 7
    OnChange = edtSepSearchChange
  end
end
