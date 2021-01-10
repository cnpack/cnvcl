object FormLockFree: TFormLockFree
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'Lock Free Containers'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnTestNoCritical: TButton
    Left = 32
    Top = 32
    Width = 89
    Height = 25
    Caption = 'Test No Critical'
    TabOrder = 0
    OnClick = btnTestNoCriticalClick
  end
  object btnTestCritical: TButton
    Left = 144
    Top = 32
    Width = 89
    Height = 25
    Caption = 'Test Critical'
    TabOrder = 1
    OnClick = btnTestCriticalClick
  end
  object btnTestSysCritical: TButton
    Left = 256
    Top = 32
    Width = 89
    Height = 25
    Caption = 'Test Sys Critical'
    TabOrder = 2
    OnClick = btnTestSysCriticalClick
  end
  object btnTestLockFreeLinkedList: TButton
    Left = 504
    Top = 32
    Width = 153
    Height = 25
    Caption = 'Test Lock-Free LinkedList'
    TabOrder = 3
    OnClick = btnTestLockFreeLinkedListClick
  end
end
