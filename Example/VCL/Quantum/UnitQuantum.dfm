object FormQuantum: TFormQuantum
  Left = 234
  Top = 171
  Width = 979
  Height = 563
  Caption = 'Quantum Test'
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
  object pbQ: TPaintBox
    Left = 24
    Top = 24
    Width = 300
    Height = 300
    OnPaint = pbQPaint
  end
  object lblSetTheta: TLabel
    Left = 344
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Theta:'
  end
  object lblSetPhi: TLabel
    Left = 344
    Top = 64
    Width = 18
    Height = 13
    Caption = 'Phi:'
  end
  object lblQ: TLabel
    Left = 344
    Top = 96
    Width = 3
    Height = 13
  end
  object lblOutQ: TLabel
    Left = 344
    Top = 176
    Width = 3
    Height = 13
  end
  object lblNewTheta: TLabel
    Left = 720
    Top = 144
    Width = 56
    Height = 13
    Caption = 'New Theta:'
  end
  object edtSetTheta: TEdit
    Left = 384
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '1.1'
  end
  object edtSetPhi: TEdit
    Left = 384
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0.6'
  end
  object btnSetTheta: TButton
    Left = 512
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Set'
    TabOrder = 2
    OnClick = btnSetThetaClick
  end
  object btnSetPhi: TButton
    Left = 512
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Set'
    TabOrder = 3
    OnClick = btnSetPhiClick
  end
  object btnHadamard: TButton
    Left = 344
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Hadamard'
    TabOrder = 4
    OnClick = btnHadamardClick
  end
  object btnPauliX: TButton
    Left = 440
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Pauli-X'
    TabOrder = 5
    OnClick = btnPauliXClick
  end
  object btnPauliY: TButton
    Left = 536
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Pauli-Y'
    TabOrder = 6
    OnClick = btnPauliYClick
  end
  object btnPauliZ: TButton
    Left = 632
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Pauli-Z'
    TabOrder = 7
    OnClick = btnPauliZClick
  end
  object edtNewTheta: TEdit
    Left = 784
    Top = 136
    Width = 81
    Height = 21
    TabOrder = 8
    Text = '1'
  end
  object btnPhaseShift: TButton
    Left = 880
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Phase Shift'
    TabOrder = 9
    OnClick = btnPhaseShiftClick
  end
end
