object FormJSON: TFormJSON
  Left = 192
  Top = 138
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'JSON Test'
  ClientHeight = 587
  ClientWidth = 1001
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
    Width = 985
    Height = 569
    ActivePage = tsJSONParse
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsJSONParse: TTabSheet
      Caption = 'JSON Parse'
      object lblReconstruct: TLabel
        Left = 464
        Top = 280
        Width = 61
        Height = 13
        Caption = 'Reconstruct:'
      end
      object lblToken: TLabel
        Left = 8
        Top = 280
        Width = 34
        Height = 13
        Caption = 'Token:'
      end
      object lblStr: TLabel
        Left = 16
        Top = 16
        Width = 30
        Height = 13
        Caption = 'String:'
      end
      object mmoJSONReconstruct: TMemo
        Left = 544
        Top = 280
        Width = 417
        Height = 241
        ReadOnly = True
        TabOrder = 10
      end
      object chkJSONFormat: TCheckBox
        Left = 456
        Top = 256
        Width = 73
        Height = 17
        Caption = 'Format'
        TabOrder = 8
      end
      object tvJSON: TTreeView
        Left = 544
        Top = 16
        Width = 417
        Height = 241
        Indent = 19
        TabOrder = 2
      end
      object mmoJSON: TMemo
        Left = 56
        Top = 16
        Width = 385
        Height = 241
        Lines.Strings = (
          '{'
          '    "animals": {'
          '        "dog": ['
          '            {'
          '                "name": "Ru吃饭fus",'
          '                "age":15,'
          '"height": 1.63'
          '            },'
          '            {'
          '                "name": "Ma\u996drty",'
          '                "age": null,'
          '    "die": true'
          '            }'
          '        ]'
          '    }'
          '}')
        TabOrder = 0
      end
      object btnParse: TButton
        Left = 456
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Parse'
        TabOrder = 1
        OnClick = btnParseClick
      end
      object mmoJSONToken: TMemo
        Left = 56
        Top = 280
        Width = 385
        Height = 241
        ScrollBars = ssVertical
        TabOrder = 9
      end
      object btnClone: TButton
        Left = 456
        Top = 80
        Width = 75
        Height = 25
        Caption = 'Clone'
        TabOrder = 4
        OnClick = btnCloneClick
      end
      object btnArray: TButton
        Left = 456
        Top = 120
        Width = 75
        Height = 25
        Caption = 'Array Test'
        TabOrder = 5
        OnClick = btnArrayClick
      end
      object btnMerge: TButton
        Left = 456
        Top = 176
        Width = 75
        Height = 25
        Caption = 'Merge Test'
        TabOrder = 6
        OnClick = btnMergeClick
      end
      object chkReplaceName: TCheckBox
        Left = 464
        Top = 208
        Width = 65
        Height = 17
        Caption = 'Replace'
        TabOrder = 7
      end
      object btnParseMulti: TButton
        Left = 456
        Top = 47
        Width = 75
        Height = 25
        Caption = 'Parse Multi'
        TabOrder = 3
        OnClick = btnParseMultiClick
      end
      object btnParseMultiStep: TButton
        Left = 456
        Top = 311
        Width = 75
        Height = 25
        Caption = 'Multi Step'
        TabOrder = 11
        OnClick = btnParseMultiStepClick
      end
      object btnParseToArray: TButton
        Left = 456
        Top = 360
        Width = 75
        Height = 25
        Caption = 'btnParseToArray'
        TabOrder = 12
        OnClick = btnParseToArrayClick
      end
    end
    object tsJSONConstruct: TTabSheet
      Caption = 'JSON Construct'
      ImageIndex = 1
      object btnJSONConstruct1: TButton
        Left = 16
        Top = 16
        Width = 113
        Height = 25
        Caption = 'JSON Construct 1'
        TabOrder = 0
        OnClick = btnJSONConstruct1Click
      end
      object mmoOutput: TMemo
        Left = 16
        Top = 64
        Width = 537
        Height = 289
        ReadOnly = True
        TabOrder = 1
      end
      object chkConstructFormat: TCheckBox
        Left = 16
        Top = 368
        Width = 97
        Height = 17
        Caption = 'Format'
        TabOrder = 2
      end
      object btnWrite: TButton
        Left = 152
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Write Form'
        TabOrder = 3
        OnClick = btnWriteClick
      end
      object btnComponent: TButton
        Left = 256
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Component'
        TabOrder = 4
        OnClick = btnComponentClick
      end
      object btnGenHuge: TButton
        Left = 552
        Top = 16
        Width = 113
        Height = 25
        Caption = 'Gen Huge JSON'
        TabOrder = 5
        OnClick = btnGenHugeClick
      end
      object btnOpenHuge: TButton
        Left = 680
        Top = 16
        Width = 97
        Height = 25
        Caption = 'Open Huge'
        TabOrder = 6
        OnClick = btnOpenHugeClick
      end
      object btnJSONSort: TButton
        Left = 360
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Sort'
        TabOrder = 7
        OnClick = btnJSONSortClick
      end
    end
    object tsNDJsonParse: TTabSheet
      Caption = 'ND JSON Parse'
      ImageIndex = 2
      object lblNDStr: TLabel
        Left = 16
        Top = 16
        Width = 30
        Height = 13
        Caption = 'String:'
      end
      object mmoNDJSON: TMemo
        Left = 56
        Top = 16
        Width = 889
        Height = 145
        Lines.Strings = (
          'event: message_start'
          
            'data: {"type":"message_start","message":{"id":"055290091b3169433' +
            '1ac6d15640ecf4d","type":"message","role":"assistant","content":[' +
            '],"model":"MiniMax-M2","stop_reason":null,"stop_sequence":null,"' +
            'usage":{"input_tokens":59,"output_tokens":0},"service_tier":"sta' +
            'ndard"}}'
          'event: ping'
          'data: {"type":"ping"}'
          'event: content_block_start'
          
            'data: {"type":"content_block_start","index":0,"content_block":{"' +
            'type":"thinking","thinking":""}}'
          'event: content_block_delta'
          
            'data: {"type":"content_block_delta","index":0,"delta":{"type":"t' +
            'hinking_delta","thinking":"首先，用户"}}'
          'event: content_block_delta'
          
            'data: {"type":"content_block_delta","index":0,"delta":{"type":"t' +
            'hinking_delta","thinking":"要求用简体中文解释代码：`Application.' +
            'CreateForm(TForm1, Form1);`。这是Delphi和Lazarus中常见的"}}')
        ScrollBars = ssBoth
        TabOrder = 0
        WantReturns = False
        WordWrap = False
      end
      object btnNDParse: TButton
        Left = 56
        Top = 176
        Width = 75
        Height = 25
        Caption = 'Parse'
        TabOrder = 1
        OnClick = btnNDParseClick
      end
      object mmoNDJsonRes: TMemo
        Left = 56
        Top = 216
        Width = 889
        Height = 145
        TabOrder = 2
      end
    end
  end
  object dlgSave1: TSaveDialog
    Left = 616
    Top = 112
  end
  object dlgOpen1: TOpenDialog
    Left = 724
    Top = 112
  end
end
