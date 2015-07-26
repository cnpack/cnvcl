object CnMemoForm: TCnMemoForm
  Left = 192
  Top = 108
  Width = 792
  Height = 474
  Caption = 'CnMemo Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object chkShowLineNumber: TCheckBox
    Left = 16
    Top = 24
    Width = 121
    Height = 17
    Caption = 'Show LineNumber'
    TabOrder = 0
    OnClick = chkShowLineNumberClick
  end
  object chkHilightLineNumber: TCheckBox
    Left = 152
    Top = 24
    Width = 161
    Height = 17
    Caption = 'Highlight Current LineNumber'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = chkHilightLineNumberClick
  end
  object btnChangeFont: TButton
    Left = 680
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Change Font'
    TabOrder = 2
    OnClick = btnChangeFontClick
  end
  object dlgFontMemo: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 552
    Top = 24
  end
end
