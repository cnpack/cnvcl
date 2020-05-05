object FormRtlUtils: TFormRtlUtils
  Left = 204
  Top = 75
  Width = 798
  Height = 603
  Caption = 'Test RTL Utils'
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
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 766
    Height = 552
    ActivePage = tsModule
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsModule: TTabSheet
      Caption = 'Module'
      object grpModule: TGroupBox
        Left = 8
        Top = 4
        Width = 742
        Height = 508
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Module Info'
        TabOrder = 0
        object btnGetMyModule: TButton
          Left = 24
          Top = 32
          Width = 161
          Height = 25
          Caption = 'Get My Modules'
          TabOrder = 0
          OnClick = btnGetMyModuleClick
        end
        object mmoMyModules: TMemo
          Left = 24
          Top = 72
          Width = 694
          Height = 417
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
        end
      end
    end
    object tsStack: TTabSheet
      Caption = 'Stack'
      ImageIndex = 1
      object grpStack: TGroupBox
        Left = 8
        Top = 4
        Width = 742
        Height = 508
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Module Info'
        TabOrder = 0
        object btnGetStack: TButton
          Left = 24
          Top = 32
          Width = 161
          Height = 25
          Caption = 'Get My Stacks'
          TabOrder = 0
          OnClick = btnGetStackClick
        end
        object mmoStack: TMemo
          Left = 24
          Top = 72
          Width = 694
          Height = 417
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 1
          WordWrap = False
        end
      end
    end
    object tsIATHook: TTabSheet
      Caption = 'IAT Hook'
      ImageIndex = 2
      object grpIATHook: TGroupBox
        Left = 8
        Top = 4
        Width = 742
        Height = 508
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Hook API by Replacing IAT '
        TabOrder = 0
        object btnHookIAT: TButton
          Left = 24
          Top = 32
          Width = 161
          Height = 25
          Caption = 'Hook MessageBoxA'
          TabOrder = 0
          OnClick = btnHookIATClick
        end
        object btnUnHookIAT: TButton
          Left = 232
          Top = 32
          Width = 161
          Height = 25
          Caption = 'UnHook MessageBoxA'
          TabOrder = 1
          OnClick = btnUnHookIATClick
        end
        object btnCallMessageBox: TButton
          Left = 552
          Top = 32
          Width = 161
          Height = 25
          Caption = 'Call MessageBoxA'
          TabOrder = 2
          OnClick = btnCallMessageBoxClick
        end
        object btnJCLHookMessageBoxA: TButton
          Left = 24
          Top = 80
          Width = 161
          Height = 25
          Caption = 'Hook MessageBoxA'
          TabOrder = 3
          OnClick = btnJCLHookMessageBoxAClick
        end
      end
    end
  end
end
