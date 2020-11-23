object FormDFT: TFormDFT
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'DFT - Discrete Fourier Transform'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnDFTButterFly: TButton
    Left = 24
    Top = 24
    Width = 89
    Height = 25
    Caption = 'DFT ButterFly'
    TabOrder = 0
    OnClick = btnDFTButterFlyClick
  end
  object edtButterFly: TEdit
    Left = 128
    Top = 24
    Width = 753
    Height = 21
    TabOrder = 1
  end
  object btnTwiddleFactors: TButton
    Left = 24
    Top = 72
    Width = 89
    Height = 25
    Caption = 'Twiddle Factors'
    TabOrder = 2
    OnClick = btnTwiddleFactorsClick
  end
  object edtTwiddleFactors: TEdit
    Left = 128
    Top = 72
    Width = 753
    Height = 21
    TabOrder = 3
  end
end
