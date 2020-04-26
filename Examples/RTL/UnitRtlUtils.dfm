object FormRtlUtils: TFormRtlUtils
  Left = 208
  Top = 123
  Width = 969
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
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 937
    Height = 552
    ActivePage = tsModule
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsModule: TTabSheet
      Caption = 'Module'
      object grpModule: TGroupBox
        Left = 8
        Top = 4
        Width = 913
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
          Width = 865
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
