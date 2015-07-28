object CnMemoForm: TCnMemoForm
  Left = 201
  Top = 195
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
  object lblLeftMargin: TLabel
    Left = 344
    Top = 24
    Width = 53
    Height = 13
    Caption = 'Left Margin'
  end
  object lblRightMargin: TLabel
    Left = 480
    Top = 24
    Width = 60
    Height = 13
    Caption = 'Right Margin'
  end
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
  object seLeftMargin: TCnSpinEdit
    Left = 416
    Top = 24
    Width = 41
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = seLeftMarginChange
  end
  object seRightMargin: TCnSpinEdit
    Left = 552
    Top = 24
    Width = 41
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = seRightMarginChange
  end
  object grpColors: TGroupBox
    Left = 568
    Top = 72
    Width = 185
    Height = 153
    Caption = 'Colors'
    TabOrder = 5
    object btnLineBkColor: TButton
      Left = 16
      Top = 24
      Width = 153
      Height = 25
      Caption = 'Line Number Background'
      TabOrder = 0
      OnClick = btnLineBkColorClick
    end
    object btnLineNumberColor: TButton
      Left = 16
      Top = 64
      Width = 153
      Height = 25
      Caption = 'Line Number'
      TabOrder = 1
      OnClick = btnLineNumberColorClick
    end
    object btnLineNumberHighlight: TButton
      Left = 16
      Top = 104
      Width = 153
      Height = 25
      Caption = 'Line Number Highlight'
      TabOrder = 2
      OnClick = btnLineNumberHighlightClick
    end
  end
  object dlgFontMemo: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 720
    Top = 72
  end
  object dlgColor: TColorDialog
    Ctl3D = True
    Left = 384
    Top = 256
  end
end
