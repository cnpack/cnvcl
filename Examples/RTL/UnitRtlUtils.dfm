object FormRtlUtils: TFormRtlUtils
  Left = 229
  Top = 86
  Width = 798
  Height = 554
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
    Height = 503
    ActivePage = tsStack
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsModule: TTabSheet
      Caption = 'Module'
      object grpModule: TGroupBox
        Left = 8
        Top = 4
        Width = 742
        Height = 459
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
        Height = 459
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
  end
end
