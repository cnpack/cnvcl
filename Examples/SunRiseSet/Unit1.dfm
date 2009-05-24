object Form1: TForm1
  Left = 355
  Top = 282
  BorderStyle = bsDialog
  Caption = 'Sun Rise Set Time'
  ClientHeight = 178
  ClientWidth = 469
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
  object lbl1: TLabel
    Left = 8
    Top = 20
    Width = 50
    Height = 13
    Caption = 'Longitude:'
  end
  object lbl2: TLabel
    Left = 8
    Top = 52
    Width = 41
    Height = 13
    Caption = 'Latitude:'
  end
  object lbl3: TLabel
    Left = 8
    Top = 84
    Width = 54
    Height = 13
    Caption = 'Zone Time:'
  end
  object lbl4: TLabel
    Left = 8
    Top = 116
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object lbl6: TLabel
    Left = 240
    Top = 20
    Width = 50
    Height = 13
    Caption = 'Rise Time:'
  end
  object lbl7: TLabel
    Left = 240
    Top = 52
    Width = 58
    Height = 13
    Caption = 'Transit Time'
  end
  object lbl8: TLabel
    Left = 240
    Top = 84
    Width = 45
    Height = 13
    Caption = 'Set Time:'
  end
  object lbl5: TLabel
    Left = 240
    Top = 116
    Width = 27
    Height = 13
    Caption = 'Type:'
  end
  object edt1: TEdit
    Left = 72
    Top = 16
    Width = 153
    Height = 21
    TabOrder = 0
    Text = '113.82'
  end
  object btn1: TButton
    Left = 384
    Top = 142
    Width = 75
    Height = 25
    Caption = '&Calculate'
    TabOrder = 3
    OnClick = btn1Click
  end
  object edt2: TEdit
    Left = 72
    Top = 48
    Width = 153
    Height = 21
    TabOrder = 1
    Text = '34.01'
  end
  object edt3: TEdit
    Left = 72
    Top = 80
    Width = 153
    Height = 21
    TabOrder = 2
    Text = '8'
  end
  object dtp1: TDateTimePicker
    Left = 72
    Top = 112
    Width = 153
    Height = 21
    CalAlignment = dtaLeft
    Date = 38701.3545812268
    Time = 38701.3545812268
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 4
  end
  object edt4: TEdit
    Left = 304
    Top = 16
    Width = 153
    Height = 21
    TabOrder = 5
  end
  object edt5: TEdit
    Left = 304
    Top = 48
    Width = 153
    Height = 21
    TabOrder = 6
  end
  object edt6: TEdit
    Left = 304
    Top = 80
    Width = 153
    Height = 21
    TabOrder = 7
  end
  object edt7: TEdit
    Left = 304
    Top = 112
    Width = 153
    Height = 21
    TabOrder = 8
  end
end
