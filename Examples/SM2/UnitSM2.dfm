object FormSM2: TFormSM2
  Left = 259
  Top = 211
  Width = 967
  Height = 588
  Caption = 'SM2 Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgcSm2: TPageControl
    Left = 16
    Top = 16
    Width = 921
    Height = 521
    ActivePage = tsEncDec
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsEncDec: TTabSheet
      Caption = 'Encryttion / Decryption'
    end
    object tsSignVerify: TTabSheet
      Caption = 'Sign / Verify'
      ImageIndex = 1
    end
  end
end
