object ProgressForm: TProgressForm
  Left = 265
  Top = 265
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'Progress'
  ClientHeight = 88
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 0
    Top = 0
    Width = 322
    Height = 88
    Enabled = False
  end
  object lblTitle: TLabel
    Left = 9
    Top = 20
    Width = 30
    Height = 13
    Caption = 'lblTitle'
  end
  object ProgressBar: TProgressBar
    Left = 9
    Top = 52
    Width = 304
    Height = 25
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 0
  end
end
