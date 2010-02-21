object ProgressForm: TProgressForm
  Left = 265
  Top = 265
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 81
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 12
  object SpeedButton1: TSpeedButton
    Left = 0
    Top = 0
    Width = 297
    Height = 81
    Enabled = False
  end
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 12
    Caption = 'lblTitle'
  end
  object Label1: TLabel
    Left = 8
    Top = 27
    Width = 54
    Height = 12
    Caption = 'Please Wait...'
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 48
    Width = 281
    Height = 23
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 0
  end
end
