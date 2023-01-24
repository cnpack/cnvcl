object Form1: TForm1
  Left = 461
  Top = 293
  Width = 388
  Height = 246
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object edtTest: TEdit
    Left = 16
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object chkEnable: TCheckBox
    Left = 16
    Top = 72
    Width = 97
    Height = 17
    Caption = 'chkEnable'
    TabOrder = 1
  end
  object chkChecked: TCheckBox
    Left = 16
    Top = 104
    Width = 97
    Height = 17
    Caption = 'chkChecked'
    TabOrder = 2
  end
  object dtpDate: TDateTimePicker
    Left = 160
    Top = 16
    Width = 89
    Height = 21
    CalAlignment = dtaLeft
    Date = 40486.5115592708
    Time = 40486.5115592708
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 3
  end
  object dtpTime: TDateTimePicker
    Left = 256
    Top = 16
    Width = 89
    Height = 21
    CalAlignment = dtaLeft
    Date = 40486.5115592708
    Time = 40486.5115592708
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkTime
    ParseInput = False
    TabOrder = 4
  end
end
