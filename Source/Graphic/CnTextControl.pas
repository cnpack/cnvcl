{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTextControl;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ��ı���ʾ��༭�ؼ�ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע���õ�Ԫ��ǰ��Ϊ�ڲ��ο�������
* ���˵����ScreenLineNumber���� ScreenRow��: �ؼ�����ʾ�õ������кţ��������� 1���ø������
*           LineNumber���� Row��: ������������кţ�1 ��ʼ���� ScreenLineNumber ��� FVertOffset
*           PaintLineNumber: ���к����ϻ��Ƶ��кţ�û���۵�������µ��� LineNumber
*           �ؼ�����ʾ��Ӧ���������кŵĸ����Ϊ�������ַ�����һ���ȿ�������ֻ����ȿ����޺��ֵ�����
*           ColNumber���� Col����������������кţ�1 ��ʼ���ͻ����е� ScreenColNumber ��� FHoriOffset
*           CaretRow����ǰ������ڵ������кţ�1 ��ʼ������ ScreenLineNumber
*           CaretCol����ǰ������ڵ������кţ���ߵ� 1 ���ַ�������ǵ� 1 �����λ��
*           ������Ϊ���϶��������������������ͷ��������ʱ��������λ�þ���Ӧ�仯
*                     ���������ҳ��ʱ��������λ�÷����仯�������ݹ������ɼ�
*           ���������ؼ�������������ʾ�������ֵ��������Ͻ�ԭ��Ϊ�ؼ��к����ұߣ�
*                 �붥���п�϶����������ԭ���ǿؼ������������Ͻ�
*           �����������������������ؼ����������ʾ������������ԭ��Ҳ�ǿؼ������������Ͻ�
*                 �ڲ��������Ű�Ϊ���壬���������� 1 ��ʼ�����Ͻǿ����ڿؼ�����������
*           �����Ű�������С��״��ͬ���������������������Ͻ�Ϊԭ��
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ�PWinXP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2021.04.20 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Messages, Windows, Graphics;

type
  ECnTextControlException = class(Exception);

  TCnVirtualTextControl = class(TCustomControl)
  {* �ܹ���ʾ��ͬ��������ֲ������Ļ��࣬�;����ַ��������޹�}
  private
    FCharFrameSize: TPoint;       // �����ַ������ߴ磬��������
    FShowLineNumber: Boolean;     // �Ƿ���ʾ�к���
    FLineNumCount: Integer;       // ����кŵ�λ��
    FLineNumPattern: string;      // ��������к�����Ӧ���ַ����������� 0000
    FLineNumWidth: Integer;       // ��������к�����Ӧ���ַ������
    FMaxLineCount: Integer;       // ��������кţ���������ã������� 1 �� FMaxLineCount
    FWheelLinesCount: Integer;    // ����������������������
    FLineNumColor: TColor;        // �к��ֵ���ɫ
    FLineNumFocusBkColor: TColor;     // �н���ʱ�к����ı���ɫ
    FLineNumNoFocusBkColor: TColor;   // �޽���ʱ�к����ı���ɫ
    FGutterRect: TRect;               // �����
    FTextRect: TRect;                 // ���ֻ�����
    FUseCaret: Boolean;               // �Ƿ�ʹ�ù��
    FCaretVisible: Boolean;           // ����Ƿ�ɼ�
    FCharFrameIsLeft: Boolean;        // �Ƿ��ڵ�ǰ�ַ��������
    FCharFrameDoubleWidth: Boolean;   // ��ǰ�ַ����Ƿ�˫�������ڿ��ֽ��ַ�
    FCaretRow: Integer;               // ������ڵ������кţ�1 ��ʼ���� LineNumber
    FCaretCol: Integer;               // ������ڵ������кţ�1 ��ʼ
    FCaretAfterLineEnd: Boolean;      // �Ƿ�������Խ����β
    FLeftMouseDown: Boolean;          // ��¼�������������
    FLeftMouseMoveAfterDown: Boolean; // ��¼���������º��Ƿ��϶���
    FIsWheeling: Boolean;             // ��ǰ����ʱ�Ƿ������������¼�����
    FOnCaretChange: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FUseSelection: Boolean;
    FOnSelectChange: TNotifyEvent;
    procedure UpdateScrollBars;       // ������Ļ״��ȷ����������λ�óߴ��
    procedure UpdateRects;
    {* �����ı������к����ȵĳߴ磬ע���� Paint ��û��ʹ��}
    procedure CalcMetrics;
    {* ����ı�ʱ���ã�ȫ������}
    function GetVisibleLineCount: Integer;
    function GetScreenBottomLine: Integer;
    procedure SetMaxLineCount(const Value: Integer);
    procedure SetShowLineNumber(const Value: Boolean);
    procedure SetLineNumColor(const Value: TColor);
    procedure SetLineNumFocusBkColor(const Value: TColor);
    procedure SetLineNumNoFocusBkColor(const Value: TColor);
    procedure SetUseCaret(const Value: Boolean);

    procedure DisplayCaret(ACaretVisible: Boolean);
    {* ���ƹ����ʾ���}
    function GetTextRectLeft: Integer;
    {* ��̬�����ı���ʾ��������ڿؼ����Ͻǵ����������꣬�����к���ʾ��������}
    function GetTextRect: TRect;
    {* ������������ı�����ʾ���������Կؼ���������Ϊ��������ȥ���ϡ����±߽�}
    function ScreenLineNumberToLineNumber(ScreenLineNumber: Integer): Integer;
    {* ����Ļ�ϵ������кţ�1 ��ʼ�ģ�ת���ɹ�����������кţ�1 ��ʼ�ģ�}
    function ScreenColNumberToColNumber(ScreenColNumber: Integer): Integer;
    {* ����Ļ�ϵ������кţ�1 ��ʼ�ģ�ת���ɹ�����������кţ�1 ��ʼ�ģ�}
    function LineNumberToScreenLineNumber(LineNumber: Integer): Integer;
    {* ����Ļ�ϵ������кţ�1 ��ʼ�ģ�ת������Ļ�ϵ������кţ�1 ��ʼ�ģ�}
    function ColNumberToScreenColNumber(ColNumber: Integer): Integer;
    {* ����Ļ�ϵ������кţ�1 ��ʼ�ģ�ת������Ļ�ϵ������кţ�1 ��ʼ�ģ�}
    function CalcRowCol(Pt: TPoint; out ACaretRow, ACaretCol: Integer;
      out ACharFrameIsLeft, ACharFrameDoubleWidth: Boolean): Boolean;
    {* ���ݿؼ���������������ַ�λ�ã����ؼ����Ƿ�ɹ�}
    procedure UpdateCursorFrameCaret;
    {* ���ݵ�ǰ���λ�ö�λ�ַ���λ��}
    procedure LimitRowColumnInLine(var LineNumber, Column: Integer);
    {* ���ݵ�ǰ���λ���Լ���������Լ��Ƿ������곬��β�������������������λ��}
    procedure SyncCaretPosition;
    {* ���ݵ�ǰ����������������������λ��}
    procedure CalcSelectEnd(Pt: TPoint);
    {* ���ݿؼ���������㲢����ѡ����ĩβ��������ڹ����ƶ���Χ��}

    procedure SetCaretCol(const Value: Integer);
    procedure SetCaretRow(const Value: Integer);
    procedure SetCaretAfterLineEnd(const Value: Boolean);
    procedure SetSelectEndCol(const Value: Integer);    // ������ѡ����β��
    procedure SetSelectEndRow(const Value: Integer);    // ������ѡ����β��
    procedure SetSelectStartCol(const Value: Integer);
    procedure SetSelectStartRow(const Value: Integer);
    function GetTopLine: Integer;                       // ֻ����û���е�ԭ�����в��ȿ�
    function GetBottomLine: Integer;
    function GetHoriPixelsOffset: Integer;              // ���������������
    procedure SetUseSelection(const Value: Boolean);
    procedure SetOnSelectChange(const Value: TNotifyEvent);
    function GetScreenCaretRow: Integer;
  protected
    FVertExp: Integer;            // ���������ָ�������ڿ���������̫��ʱ�������������̫ϸ
    FVertOffset: Integer;         // �������ƫ��������������Ϊ��λ��0 ��ʼ���� 1 ���� TopLine
    FHoriOffset: Integer;         // �������ƫ��������ƽ���ַ����Ϊ��λ��0 ��ʼ���ǵȿ������º��к�û��ֱ�ӹ�ϵ
    FAveCharWidth: Integer;       // ������ַ�ƽ����ȣ���������������
    FMaxCharWidth: Integer;       // ������ַ��������
    FAveCharWidthHalf: Integer;   // �����ַ������ߴ��һ�룬���жϵ��ʱ������ַ�ǰ��ʹ��
    FLineHeight: Integer;         // �иߣ�������������
    FFontIsFixedWidth: Boolean;   // �����Ƿ�ȿ�
    FSelectStartRow: Integer;     // ѡ������ʼ�����кţ�ע����ʼ�ͽ�����һ���ĸ���ǰ
    FSelectEndRow: Integer;       // ѡ�������������к�
    FSelectStartCol: Integer;     // ѡ������ʼ�����к�
    FSelectEndCol: Integer;       // ѡ�������������к�

    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSetFont(var message: TMessage); message WM_SETFONT;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;

    procedure WMVScroll(var message: TWMScroll); message WM_VSCROLL;
    {* ���������Ϣ���������������������ʼ�У���������λ��}
    procedure WMHScroll(var message: TWMScroll); message WM_HSCROLL;
    {* ���������Ϣ�������º������������ʼ�У���������λ��}

    procedure WMSetFocus(var message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMSize(var message: TWMSize); message WM_SIZE;
    procedure WMMouseWheel(var message: TWMMouseWheel); message WM_MOUSEWHEEL;
    {* �����ֹ������ö�����Ӱ������Ĺ��λ�ã�ֻ������������������λ�ÿ����Ƴ���Ļ��}

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure KeyDown(var Key: WORD; Shift: TShiftState); override;
    procedure DoScroll; virtual;
    procedure DoCaretChange; virtual;
    {* ���λ�÷����ı�ʱ����}
    procedure DoSelectChange; virtual;
    {* ѡ���������ı�ʱ����}

    procedure NavigationKey(Key: WORD; Shift: TShiftState); virtual;
    {* �յ��������·�����Լ� PageUp/PageDown Home/End ���Ĵ�������ֻ��������
      ���໹�������д�Դ������ƶ�}
    procedure ScrollToVisibleCaret;
    {* ����λ������Ļ��ʱ������Ļ�Խ����¶���������겻�䣬��������ܷ����仯
      �ڲ�ʵ�����޸� FVertOffset �� FHoriOffset ���ػ�}
    function SyncSelectionStartEnd(Force: Boolean = False): Boolean;
    {* ����ѡ��״̬ʱ����ǿ�ƣ��������λ��ͬ������ѡ����ʼ����λ�ã�Ҳ��ζ��ȡ��ѡ��
      ����ֵΪ True ʱ��ʾִ����ͬ����ȡ��ѡ����}

    procedure ScrollUpLine;      // �Ϲ�һ��
    procedure ScrollDownLine;    // �¹�һ��
    procedure ScrollLeftCol;     // ���һ�ַ����
    procedure ScrollRightCol;    // �ҹ�һ�ַ����
    procedure ScrollUpPage;      // �Ϲ�һ��
    procedure ScrollDownPage;    // �¹�һ��
    procedure ScrollLeftPage;    // ���һ��
    procedure ScrollRightPage;   // �ҹ�һ��

    procedure SetCaretRowCol(Row, Col: Integer);
    procedure MakeOrderSelection(var SelStartRow, SelStartCol, SelEndRow, SelEndCol: Integer);

    function ClientXToVirtualX(X: Integer): Integer;
    function VirtualXToClientX(X: Integer): Integer;
    {* �����Ű�����ĺ�������������ؼ��ĺ���������������ת��}

    procedure GetVirtualCharPosVirtualRect(ARow, ACol: Integer; var Rect: TRect);
    {* �����������������������Ű����ڵ� Rect�������Ű������Ͻ���ԭ��}
    function GetVirtualCharPosPhysicalRect(ARow, ACol: Integer; var Rect: TRect): Boolean;
    {* ������������������ؼ��ϵ����� Rect�������Ƿ�ɹ���ע����ܳ��� TextRect}
    function GetColumnVirtualX(ARow, ACol: Integer): Integer;
    {* ���ĳ���������������Ĺ��λ����������������������������غ�����}
    procedure Paint; override;
    {* ���Ʒ���}
    procedure DoPaintLineNum(ScreenLineNumber, LineNumber: Integer; LineNumRect: TRect); virtual;
    {* Ĭ�ϵĻ����кŵķ��������� ScreenLineNumber Ϊ 1 ��ʼ�������кţ�
      LineNumber Ϊ������������кţ�LineNumRect Ϊ�кŴ����Ƶķ���}

    procedure DoPaintLineBackground(LineCanvas: TCanvas; LineNumber: Integer;
      LineRect: TRect); virtual;
    {* �����б�����Ĭ��ʵ�����ÿؼ� Color ������ɫ���}
    procedure DoPaintLine(LineCanvas: TCanvas; LineNumber, HoriCharOffset: Integer;
      LineRect: TRect); virtual; abstract;
    {* �������ʵ�ֵĻ��������ݵķ��������� ScreenLineNumber Ϊ 1 ��ʼ�������кţ�
      LineNumber Ϊ������������кţ�HoriCharOffset Ϊ����Ѿ��������ַ�������������ϸ������������
      LineNumRect Ϊ�����ݴ����Ƶĳ�������}

    function GetPaintLineNumber(LineNumber: Integer): Integer; virtual;
    {* �ӹ�����������кŷ������к����ϻ��Ƶ��кţ�Ĭ�����ֱ�ӷ���ԭʼֵ����
      ���෵�ز�ֵͬ�Դ����۵������ע�ⷵ��ֵ��Ҫ��������к�ֵ}

    function ClientPosToVirtualCharPos(Pt: TPoint; out Row, Col: Integer;
      out LeftHalf, DoubleWidth: Boolean; ExtendOut: Boolean = True): Boolean; virtual;
    {* ���ؼ��ڵ���������ת��Ϊ�����������꣬Ҳ������������ڼ��У����ڵڼ����ַ������ڣ�
      �Լ����ַ����������ڿ���뻹�ǿ��Ұ룬����ȷ���������λ�á������Ƿ�ɹ�
      Row��Col ���� 1 ��ʼ��ExtendOut Ϊ True ʱ��ʾ�������������Ҳ���￿�����ȥ
      ע��������������һ���Ҳ�ܼ���ɹ����� True}

    function CalcColumnFromPixelOffsetInLine(ARow, VirtualX: Integer;
      out Col: Integer; out LeftHalf, DoubleWidth: Boolean): Boolean; virtual;
    {*** ���������к����������Ű����ڵĺ����������ڵ��ַ����򣨻����ַ����������ߣ������Ҳ���������кţ�1 ��ʼ��
      LeftHalf ���� X λ���Ƿ��ڴ��ַ���������ߣ�DoubleWidth ���ظ��ַ������Ƿ���˫����
      �ڲ�ʵ���ǣ��ȿ�����ʱ��ֱ�Ӹ����ַ�ƽ����� FAveCharWidth ���㣬��û���������Ƿ���
      �����Ƿ�ɹ�}

    function CalcPixelOffsetFromColumnInLine(ARow, ACol: Integer; out Rect: TRect;
      out DoubleWidth: Boolean): Boolean; virtual;
    {*** ���������к��������кţ����㷵�ظ��������ַ���������ڵ�ǰ�����������з����λ��
      ����˵���й������λ�õ��ұ��ַ����ַ������Լ����ַ������Ƿ���˫��������ڷ������
     ��Rect ��ԭ���ǵ�ǰ�����е����Ͻǣ��� Left ���ǹ���ڵ�ǰ�������ڵ����꣩
      �����Ƿ�ɹ���False ��ʾ���в����ڣ�Ʃ��˫�ֽ��ַ����ɷָ}

    function GetLastColumnFromLine(LineNumber: Integer): Integer; virtual; abstract;
    {*** ���������кŻ�ø�����β��������ֵ���������ʵ�֣�һ�������ݳ��ȼ� 1���;�������޹�
      �������ݣ����뷵�� 1}

    function GetPrevColumn(AColumn, ARow: Integer): Integer; virtual;
    {** ĳ�����������ĳ�е�ǰһ�У��;�������޹�}
    function GetNextColumn(AColumn, ARow: Integer; ACaretAfterLineEnd: Boolean): Integer; virtual;
    {** ĳ�����������ĳ�еĺ�һ�У��;�������޹�}
    function GetNearestColumn(AColumn, ARow: Integer): Integer; virtual;
    {** ĳ�����������ĳ�и����ĺϷ��У������������м�ʱҪ�Ӽ�һ}
  public
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;

    constructor Create(AOwner: TComponent); override;
    {* ���캯��}
    destructor Destroy; override;
    {* ��������}

    procedure SelectRange(StartRow, StartCol, EndRow, EndCol: Integer);
    {* ѡ��ָ�����򣬹��˳���ƶ���ѡ����β}
    procedure SelectAll;
    {* ѡ��ȫ��}
    function HasSelection: Boolean;
    {* �Ƿ���ѡ�������ڣ�Ҳ�����ж���ʼ�ͽ���λ���Ƿ���ͬ}

    property LineHeight: Integer read FLineHeight;
    {* �иߣ��������ָ߶ȡ����� ExternalLeading �Լ��м仭������Ԥ���Ŀ�϶}
    property TopLine: Integer read GetTopLine;
    {* �ؼ���ʾ�������һ�е������кţ����� FVertOffset + 1}
    property BottomLine: Integer read GetBottomLine;
    {* �ؼ���ʾ����������һ�е������кţ������� FMaxLineCount
      ���� FVertOffset + 1 + (TextRect.Bottom - TextRect.Top) div FLineHeight}

    property ScreenBottomLine: Integer read GetScreenBottomLine;
    {* ������һ�е������кţ���������һ�е������к��� 1��}
    property VisibleLineCount: Integer read GetVisibleLineCount;
    {* ���ӻ���������������ʾ����߶��������и�}
    property MaxLineCount: Integer read FMaxLineCount write SetMaxLineCount;
    {* �������Ƶ������������}

    property WheelLinesCount: Integer read FWheelLinesCount write FWheelLinesCount;
    {* ������һ�ι���������}
    property ShowLineNumber: Boolean read FShowLineNumber write SetShowLineNumber;
    {* �Ƿ���ʾ�к���}
    property LineNumFocusBkColor: TColor read FLineNumFocusBkColor write SetLineNumFocusBkColor;
    {* �н���ʱ�к����ı�����ɫ}
    property LineNumNoFocusBkColor: TColor read FLineNumNoFocusBkColor write SetLineNumNoFocusBkColor;
    {* �޽���ʱ�к����ı�����ɫ}
    property LineNumColor: TColor read FLineNumColor write SetLineNumColor;
    {* �к�����������ɫ}
    property UseCaret: Boolean read FUseCaret write SetUseCaret;
    {* �Ƿ���ʾ������}
    property CaretAfterLineEnd: Boolean read FCaretAfterLineEnd write SetCaretAfterLineEnd;
    {* �Ƿ�������Խ����β}
    property ScreenCaretRow: Integer read GetScreenCaretRow;
    {* ��ǰ���λ�����ڵ���Ļ�кţ�1 ��ʼ��Ӧ�������ݹ������仯������Ĳ��䣩��
      ֵ��С�� 0����ʾ��ǰ����ڿؼ������⡣û����Ļ�кŵ�ԭ�������岻�ȿ�}
    property CaretRow: Integer read FCaretRow write SetCaretRow;
    {* ��ǰ���λ�����ڵĹ�����������кţ�1 ��ʼ������ʱ���䣬�����������
      �� ScreenCaretRow ��һ�� FVertOffset}
    property CaretCol: Integer read FCaretCol write SetCaretCol;
    {* ��ǰ���λ�����ڵĹ�����������кţ�1 ��ʼ������ʱ���䣬�����������
      �� ScreenCaretCol ��һ�� FHoriOffset}

    property UseSelection: Boolean read FUseSelection write SetUseSelection;
    {* �Ƿ�����ѡ��������}

    property SelectStartRow: Integer read FSelectStartRow write SetSelectStartRow;
    {* ѡ������ʼ�У�1 ��ʼ�������к�}
    property SelectStartCol: Integer read FSelectStartCol write SetSelectStartCol;
    {* ѡ������ʼ�У�1 ��ʼ�������к�}
    property SelectEndRow: Integer read FSelectEndRow write SetSelectEndRow;
    {* ѡ���������У�1 ��ʼ�������к�}
    property SelectEndCol: Integer read FSelectEndCol write SetSelectEndCol;
    {* ѡ���������У�1 ��ʼ�������к�}

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    {* �����¼�}
    property OnCaretChange: TNotifyEvent read FOnCaretChange write FOnCaretChange;
    {* ����ƶ��¼�}
    property OnSelectChange: TNotifyEvent read FOnSelectChange write SetOnSelectChange;
    {* ѡ���������ı�ʱ������ע���ʱ��겻һ��̧����}
  published
    property Align;
    property Ctl3D;
    property Color;
    property Font;
  end;

resourcestring
  SCnTextControlErrorColumn = 'Error Column';

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

const
  MAX_NO_EXP_LINES = 32768;
  LEFT_MARGIN = 3;             // �к�������������ߵĿ�϶
  COMMON_MARGIN = 3;           // �����������Ŀ�϶
  SEP_WIDTH = 3;               // �к������������ָ��ߵĿ��
  LINE_GAP = 2;                // ������֮��Ŀ�϶���û��»��߲�������
  CARET_MARGIN = 3;
  DEFAULT_MAX_WIDTH = 255;

function GetNumWidth(Int: Integer): Integer;
begin
  Result := Length(IntToStr(Int));
end;

function EnumFontsProc(var ELF: TEnumLogFont; var TM: TNewTextMetric;
  FontType: Integer; Data: LPARAM): Integer; stdcall;
begin;
  Result := Integer(FIXED_PITCH = (ELF.elfLogFont.lfPitchAndFamily and FIXED_PITCH));
end;

{ TCnTextControl }

procedure TCnVirtualTextControl.CalcMetrics;
const
  csAlphaText = 'abcdefghijklmnopqrstuvwxyz';
  csHeightText = 'Wj_';
  csWidthText = '1';
var
  LogFont: TLogFont;
  DC: HDC;
  SaveFont: HFONT;
  AHandle: THandle;
  TM: TEXTMETRIC;
  ASize: TSize;
begin
  FLineHeight := 0;

  if GetObject(Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
  begin
    DC := CreateCompatibleDC(0);
    SaveFont := 0;
    try
      AHandle := CreateFontIndirect(LogFont);
      AHandle := SelectObject(DC, AHandle);
      if SaveFont = 0 then
        SaveFont := AHandle
      else if AHandle <> 0 then
        DeleteObject(AHandle);

      GetTextMetrics(DC, TM);
      FAveCharWidth := TM.tmAveCharWidth; // �õ��ַ�ƽ�����
      FMaxCharWidth := TM.tmMaxCharWidth; // �õ��ַ��������

      GetTextExtentPoint(DC, csAlphaText, Length(csAlphaText), ASize);

      // ȡ�ı��߶��������и�
      if TM.tmHeight + TM.tmExternalLeading > FLineHeight then
        FLineHeight := TM.tmHeight + TM.tmExternalLeading;

      if ASize.cy > FLineHeight then
        FLineHeight := ASize.cy;

      // FLineHeight Ҫ�������¿�϶�Լ��»��ߵĿռ�����
      Inc(FLineHeight, LINE_GAP);

      // ����
      if ASize.cx div Length(csAlphaText) > FAveCharWidth then
        FAveCharWidth := ASize.cx div Length(csAlphaText);

      // ͨ����һ�ַ�ʽ����ַ���Ĵ�С
      GetTextExtentPoint32(DC, csWidthText, Length(csWidthText), ASize);
      FCharFrameSize.x := ASize.cx;
      FAveCharWidthHalf := FCharFrameSize.x shr 1;

      GetTextExtentPoint32(DC, csHeightText, Length(csHeightText), ASize);
      FCharFrameSize.y := ASize.cy;

      // �����кſ��
      GetTextExtentPoint32(DC, PChar(FLineNumPattern), Length(FLineNumPattern), ASize);
      FLineNumWidth := ASize.cx;

      // �ж��Ƿ�ȿ�
{$IFDEF DELPHI104_SYDNEY_UP}
      FFontIsFixedWidth := EnumFontFamiliesEx(DC, LogFont, @EnumFontsProc, 0, 0) = 1;
{$ELSE}
      FFontIsFixedWidth := EnumFontFamiliesEx(DC, LogFont, @EnumFontsProc, 0, 0) = BOOL(1);
{$ENDIF}
    finally
      SaveFont := SelectObject(DC, SaveFont);
      if SaveFont <> 0 then
        DeleteObject(SaveFont);
      DeleteDC(DC);
    end;
  end;
end;

function TCnVirtualTextControl.ClientPosToVirtualCharPos(Pt: TPoint; out Row,
  Col: Integer; out LeftHalf, DoubleWidth: Boolean; ExtendOut: Boolean): Boolean;
var
  TR: TRect;
  SR: Integer;
begin
  Result := False;
  TR := GetTextRect;

  if ExtendOut then
  begin
    // �����ڷ�����ʱ���жϷ���������
    if Pt.x < TR.Left then
      Pt.x := TR.Left + 1;
    if Pt.x > TR.Right then
      Pt.x := TR.Right - 1;
    if Pt.y < TR.Top then
      Pt.y := TR.Top + 1;
    if Pt.y > TR.Bottom then
      Pt.y := TR.Bottom - 1;
  end;

  if PtInRect(TR, Pt) then
  begin
    SR := ((Pt.y - TR.Top) div FLineHeight) + 1;
    Row := ScreenLineNumberToLineNumber(SR);

    // �ؼ��������ڵ���������������Ҫת�������������������������
    Result := CalcColumnFromPixelOffsetInLine(Row, ClientXToVirtualX(Pt.x), Col,
      LeftHalf, DoubleWidth);
  end;
end;

constructor TCnVirtualTextControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csCaptureMouse, csOpaque, csClickEvents, csDoubleClicks];
  SetBounds(Left, Top, 300, 200);
  ParentFont := True;
  ParentColor := False;
  TabStop := True;
  DoubleBuffered := True;

  FLineNumCount := 1;
  FLineNumPattern := '0';
  FLineHeight := 12;
  FWheelLinesCount := 3;

  FLineNumColor := clNavy;
  FLineNumFocusBkColor := clSilver;
  FLineNumNoFocusBkColor := clGray;

  FCaretRow := 1;
  FCaretCol := 1;
  FCaretAfterLineEnd := True;

  CalcMetrics;
end;

procedure TCnVirtualTextControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := Style or WS_VSCROLL or WS_HSCROLL or WS_TABSTOP;
    if NewStyleControls and Ctl3D then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else
      Style := Style or WS_BORDER;
  end;
end;

destructor TCnVirtualTextControl.Destroy;
begin

  inherited;
end;


procedure TCnVirtualTextControl.DoPaintLineNum(ScreenLineNumber,
  LineNumber: Integer; LineNumRect: TRect);
begin
  Canvas.TextOut(LineNumRect.Left, LineNumRect.Top, IntToStr(LineNumber));
end;

procedure TCnVirtualTextControl.DoScroll;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

//procedure TCnVirtualTextControl.GetScreenCharPosRect(ScreenRow,
//  ScreenCol: Integer; var Rect: TRect);
//begin
//  Rect := GetTextRect;
//  Inc(Rect.Top, (ScreenRow - 1) * FLineHeight);
//  Inc(Rect.Left, (ScreenCol - 1) * FCharFrameSize.x);
//
//  Rect.Bottom := Rect.Top + FLineHeight;
//  Rect.Right := Rect.Left + FCharFrameSize.x;
//end;

procedure TCnVirtualTextControl.GetVirtualCharPosVirtualRect(ARow,
  ACol: Integer; var Rect: TRect);
var
  DW: Boolean;
  DV: Integer;
begin
  if not CalcPixelOffsetFromColumnInLine(ARow, ACol, Rect, DW) then
    raise ECnTextControlException.Create(SCnTextControlErrorColumn);

  // �õ��� Rect ��һ���ڵ��ַ��������꣬���Ͻ�Ϊ������ͷ����Ҫת���������Ű���������
  // ע������øĶ�

  // ���ص������������������������������µ� Rect.Top
  DV := (ARow - 1) * FLineHeight;
  Inc(Rect.Top, DV);
  Inc(Rect.Bottom, DV);
end;

function TCnVirtualTextControl.GetVirtualCharPosPhysicalRect(ARow, ACol: Integer;
  var Rect: TRect): Boolean;
var
  DW: Boolean;
  DH, DV: Integer;
begin
  Result := False;
  if not CalcPixelOffsetFromColumnInLine(ARow, ACol, Rect, DW) then
    Exit;

  // �õ��� Rect ��һ���ڵ��ַ��������꣬���Ͻ�Ϊ������ͷ����Ҫת���ɿؼ�������

  // ���ص����������������������������� Rect.Top �� TextRect �ϱߵľ���
  DV := GetTextRect.Top + ((LineNumberToScreenLineNumber(ARow) - 1) * FLineHeight);
  Inc(Rect.Top, DV);
  Inc(Rect.Bottom, DV);

  DH := GetTextRect.Left - GetHoriPixelsOffset;

  // ���صĺ������ȥ������������������� Rect.Left �� TextRect ��ߵľ���
  Inc(Rect.Left, DH);
  Inc(Rect.Right, DH);
  Result := True;
end;

function TCnVirtualTextControl.CalcColumnFromPixelOffsetInLine(ARow,
  VirtualX: Integer; out Col: Integer; out LeftHalf, DoubleWidth: Boolean): Boolean;
var
  T: Integer;
begin
  // �ȿ�������������ûɶ���⣬�������ú���
  // �ǵȿ�����Ͳ��ˣ�������ú�ʵ��
  Col := (VirtualX div FAveCharWidth) + 1;

  T := VirtualX - (Col - 1) * FAveCharWidth;
  LeftHalf := T <= FAveCharWidthHalf;
  DoubleWidth := False;

  Result := True;
end;

function TCnVirtualTextControl.GetPaintLineNumber(LineNumber: Integer): Integer;
begin
  Result := LineNumber;
end;

function TCnVirtualTextControl.GetTextRect: TRect;
begin
  Result.Top := COMMON_MARGIN;
  Result.Left := GetTextRectLeft;
  Result.Bottom := Result.Top + FLineHeight * VisibleLineCount;
  Result.Right := ClientWidth;
end;

function TCnVirtualTextControl.GetTextRectLeft: Integer;
begin
  Result := LEFT_MARGIN;
  if FShowLineNumber then
    Inc(Result, LEFT_MARGIN + FLineNumWidth + COMMON_MARGIN + SEP_WIDTH);
    // �ı�����߾������ƶ����Ƶľ���Ϊ�к�����ȼ��ϻ��߿��
end;

function TCnVirtualTextControl.GetTopLine: Integer;
begin
  Result := FVertOffset + 1;
end;

function TCnVirtualTextControl.GetBottomLine: Integer;
begin
  Result := FVertOffset + GetVisibleLineCount;
  if Result > FMaxLineCount then
    Result := FMaxLineCount;
end;

function TCnVirtualTextControl.GetVisibleLineCount: Integer;
begin
  if HandleAllocated then
    Result := (ClientHeight - COMMON_MARGIN * 2) div FLineHeight
  else
    Result := 0;
end;

procedure TCnVirtualTextControl.NavigationKey(Key: WORD; Shift: TShiftState);
var
  Msg: TWMScroll;
begin
  if FUseCaret then
  begin
    // �����й��ʱ�ķ����
    if not (ssShift in Shift) then
    begin
      // û�� Shift������ѡ��ģʽ�����ƶ���꣬��ȡ��ѡ�񲢸��½���
      if SyncSelectionStartEnd(True) then
        Invalidate;

      case Key of
        VK_LEFT:
          begin
            // ���������Ʋ����ֿɼ�
            CaretCol := GetPrevColumn(CaretCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_RIGHT:
          begin
            // ���������Ʋ����ֿɼ�
            CaretCol := GetNextColumn(CaretCol, CaretRow, CaretAfterLineEnd);
            ScrollToVisibleCaret;
          end;
        VK_UP:
          begin
            // ���������Ʋ����ֿɼ�
            CaretRow := CaretRow - 1;
            CaretCol := GetNearestColumn(CaretCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_DOWN:
          begin
            // ���������Ʋ����ֿɼ�
            CaretRow := CaretRow + 1;
            CaretCol := GetNearestColumn(CaretCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_PRIOR:
          begin
            CaretRow := CaretRow - GetVisibleLineCount;
            CaretCol := GetNearestColumn(CaretCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_NEXT:
          begin
            CaretRow := CaretRow + GetVisibleLineCount;
            CaretCol := GetNearestColumn(CaretCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_HOME:
          begin
            if ssCtrl in Shift then // Ctrl ����ʱ�ص���������
              CaretRow := 1;

            CaretCol := 1; // �ص�����
            ScrollToVisibleCaret;
          end;
        VK_END:
          begin
            if ssCtrl in Shift then // Ctrl ����ʱ�ص�β��β��
              CaretRow := FMaxLineCount;

            CaretCol := GetLastColumnFromLine(FMaxLineCount); // �ص�β��
            ScrollToVisibleCaret;
          end;
      end;
    end
    else
    begin
      // ���� Shift�����ѡ�����յ�λ��
      if not FUseSelection then
        Exit;

      case Key of
        VK_LEFT:
          begin
            // ѡ�����յ������Ʋ����ֿɼ�
            SelectEndCol := GetPrevColumn(SelectEndCol, CaretRow);
            ScrollToVisibleCaret;
          end;
        VK_RIGHT:
          begin
            // ѡ�����յ������Ʋ����ֿɼ�
            SelectEndCol := GetNextColumn(SelectEndCol, CaretRow, CaretAfterLineEnd);
            ScrollToVisibleCaret;
          end;
        VK_UP:
          begin
            // ѡ�����յ������Ʋ����ֿɼ�
            SelectEndRow := SelectEndRow - 1;
            SelectEndCol := GetNearestColumn(SelectEndCol, SelectEndRow);
            ScrollToVisibleCaret;
          end;
        VK_DOWN:
          begin
            // ѡ�����յ������Ʋ����ֿɼ�
            SelectEndRow := SelectEndRow + 1;
            SelectEndCol := GetNearestColumn(SelectEndCol, SelectEndRow);
            ScrollToVisibleCaret;
          end;
        VK_PRIOR:
          begin
            SelectEndRow := SelectEndRow - GetVisibleLineCount;
            SelectEndCol := GetNearestColumn(SelectEndCol, SelectEndRow);
            ScrollToVisibleCaret;
          end;
        VK_NEXT:
          begin
            SelectEndRow := SelectEndRow + GetVisibleLineCount;
            SelectEndCol := GetNearestColumn(SelectEndCol, SelectEndRow);
            ScrollToVisibleCaret;
          end;
        VK_HOME:
          begin
            if ssCtrl in Shift then // Ctrl ����ʱѡ����������
              SelectEndRow := 1;

            SelectEndCol := 1; // ѡ������
            ScrollToVisibleCaret;
          end;
        VK_END:
          begin
            if ssCtrl in Shift then // Ctrl ����ʱѡ��β��β��
              SelectEndRow := FMaxLineCount;

            SelectEndCol := GetLastColumnFromLine(FMaxLineCount); // ѡ��β��
            ScrollToVisibleCaret;
          end;
      end;
    end;
  end
  else // û�й�꣬�����ƶ��������򣬲����� Ctrl ���ļ�ͷβ�����
  begin
    case Key of
      VK_LEFT: ScrollLeftCol;
      VK_RIGHT: ScrollRightCol;
      VK_UP: ScrollUpLine;
      VK_DOWN: ScrollDownLine;
      VK_PRIOR: ScrollUpPage;
      VK_NEXT: ScrollDownPage;
      VK_HOME:
        begin
          Msg.ScrollCode := SB_THUMBTRACK;
          Msg.Pos := 0;
          WMHScroll(Msg);

          if ssCtrl in Shift then
          begin
            Msg.ScrollCode := SB_THUMBTRACK;
            Msg.Pos := 0;
            WMVScroll(Msg);
          end;
        end;
      VK_END:
        begin
          if ssCtrl in Shift then
          begin
            Msg.ScrollCode := SB_THUMBTRACK;
            Msg.Pos := FMaxLineCount;
            WMVScroll(Msg);
          end;

          Msg.ScrollCode := SB_THUMBTRACK;
          Msg.Pos := DEFAULT_MAX_WIDTH;
          WMHScroll(Msg);
        end;
    end;
  end;
end;

procedure TCnVirtualTextControl.KeyDown(var Key: WORD; Shift: TShiftState);
begin
  inherited;
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);

  case Key of
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END, VK_LEFT, VK_RIGHT:
      NavigationKey(Key, Shift);
  end;
end;

procedure TCnVirtualTextControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if not Focused then
    Windows.SetFocus(Handle);

  UpdateCursorFrameCaret;
  ScrollToVisibleCaret;

  if Button = mbLeft then
  begin
    FLeftMouseDown := True;
    FLeftMouseMoveAfterDown := False;
  end;
end;

procedure TCnVirtualTextControl.Paint;
var
  TR, LR, LineRect: TRect;
  LC: TColor;
  I, V, H: Integer;
  LineBmp: TBitmap;
  LineCanvas: TCanvas;
begin
  // �Ȼ��к���
  TR := ClientRect;
  LR := ClientRect;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(TR);

  TR.Left := GetTextRectLeft;
  TR.Top := COMMON_MARGIN;
  V := VisibleLineCount;
  H := GetHoriPixelsOffset;

  if FShowLineNumber then // �����к����ĵ�ɫ������Ĭ�ϵ��кŻ���
  begin
    if Focused then
      LC := FLineNumFocusBkColor
    else
      LC := FLineNumNoFocusBkColor;

    // �к������Ϊ Margin + LineNumWidth + Margin
    // ������������ Left Ϊ Margin��Width Ϊ LineNumWidth + Margin

    LR.Right := LEFT_MARGIN + FLineNumWidth + LEFT_MARGIN + SEP_WIDTH;  // SEP_WIDTH �ǻ��߿��

    Canvas.Brush.Color := LC;
    Canvas.FillRect(LR);

    Inc(LR.Left, LEFT_MARGIN);
    Inc(LR.Top, COMMON_MARGIN);

    Canvas.Pen.Color := clGray;
    Dec(LR.Right);
    Canvas.MoveTo(LR.Right, 0);
    Canvas.LineTo(LR.Right, ClientHeight);
    Canvas.Pen.Color := clWhite;
    Dec(LR.Right);
    Canvas.MoveTo(LR.Right, 0);
    Canvas.LineTo(LR.Right, ClientHeight);
    Canvas.Pen.Color := clSilver;
    Dec(LR.Right);
    Canvas.MoveTo(LR.Right, 0);
    Canvas.LineTo(LR.Right, ClientHeight);

    LR.Bottom := COMMON_MARGIN + FLineHeight;

    Canvas.Font.Color := FLineNumColor;
    Canvas.Brush.Style := bsClear;

    for I := 1 to V do
    begin
      if I + FVertOffset <= FMaxLineCount then
        DoPaintLineNum(I, GetPaintLineNumber(I + FVertOffset), LR);

      Inc(LR.Top, FLineHeight);
      Inc(LR.Bottom, FLineHeight);
    end;
  end;

  TR.Bottom := TR.Top + FLineHeight;
  // TR �ǿؼ��ڲ�Ҫ���Ƶĵ�ǰ�е������

  LineBmp := TBitmap.Create;
  LineBmp.PixelFormat := pf24bit;
  LineBmp.Height := FLineHeight;
  LineBmp.Width := H + (FTextRect.Right - FTextRect.Left);

  LineCanvas := LineBmp.Canvas;
  LineCanvas.Font.Assign(Font);

  try
    for I := 1 to V do // I ��������
    begin
      LineRect := Bounds(0, 0, LineBmp.Width, LineBmp.Height);

      // ���� LineBmp Background
      DoPaintLineBackground(LineCanvas, I + FVertOffset, LineRect);

      // �����������
      DoPaintLine(LineCanvas, I + FVertOffset, FHoriOffset, LineRect);

      // ���ݸ��ƹ�ȥ
      BitBlt(Canvas.Handle, TR.Left, TR.Top, LineBmp.Width - H,
        LineBmp.Height, LineCanvas.Handle, H, 0, SRCCOPY);

      Inc(TR.Top, FLineHeight);
      Inc(TR.Bottom, FLineHeight);
    end;
  finally
    LineBmp.Free;
  end;
end;

function TCnVirtualTextControl.ScreenLineNumberToLineNumber(
  ScreenLineNumber: Integer): Integer;
begin
  Result := ScreenLineNumber + FVertOffset;
end;

procedure TCnVirtualTextControl.SetLineNumColor(const Value: TColor);
begin
  if FLineNumColor <> Value then
  begin
    FLineNumColor := Value;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetLineNumFocusBkColor(const Value: TColor);
begin
  if FLineNumFocusBkColor <> Value then
  begin
    FLineNumFocusBkColor := Value;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetLineNumNoFocusBkColor(const Value: TColor);
begin
  if FLineNumNoFocusBkColor <> Value then
  begin
    FLineNumNoFocusBkColor := Value;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetMaxLineCount(const Value: Integer);
var
  Old: Integer;
begin
  if FMaxLineCount <> Value then
  begin
    FMaxLineCount := Value;

    Old := FLineNumCount;
    FLineNumCount := GetNumWidth(Value);
    FLineNumPattern := StringOfChar('0', FLineNumCount);

    if FLineNumCount <> Old then // λ�������仯�����¼��� FLineNumWidth
    begin
      CalcMetrics;
      UpdateRects;
      UpdateScrollBars;
    end;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.SetShowLineNumber(const Value: Boolean);
begin
  if FShowLineNumber <> Value then
  begin
    FShowLineNumber := Value;
    UpdateRects;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.UpdateScrollBars;
var
  SI: TScrollInfo;
begin
  if not HandleAllocated then
    Exit;

  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_RANGE or SIF_POS or SIF_PAGE;
  SI.nMin := 0;

  // ���������
  FVertExp := 0;
  SI.nMax := FMaxLineCount - 1;       // nMax ���������
  while SI.nMax > MAX_NO_EXP_LINES do // �к�̫��ʱ��ָ����ʽ����������̫ϸ
  begin
    SI.nMax := SI.nMax div 2;
    Inc(FVertExp);
  end;

  SI.nPage := VisibleLineCount shr FVertExp; // nPage ��һ�����ݶ�Ӧ�ĸ߶�
  SI.nPos := FVertOffset shr FVertExp;
  SetScrollInfo(Handle, SB_VERT, SI, True);

  // ���������
  SI.nMax := DEFAULT_MAX_WIDTH;              // ��֪�����д�� 256 ��
  SI.nPage := ClientWidth div FAveCharWidth;    // nPage ��һ�����ݶ�Ӧ���ַ����
  SI.nPos := FHoriOffset;
  SetScrollInfo(Handle, SB_HORZ, SI, True);
end;

procedure TCnVirtualTextControl.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TCnVirtualTextControl.WMHScroll(var message: TWMScroll);
var
  SI: TScrollInfo;
  Old: Integer;
begin
  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  GetScrollInfo(Handle, SB_HORZ, SI);

  Old := FHoriOffset;
  case message.ScrollCode of
    SB_PAGEUP: Dec(FHoriOffset, (FTextRect.Right - FTextRect.Left) div FAveCharWidth);  // ����һ���Ŀ��
    SB_PAGEDOWN: Inc(FHoriOffset, (FTextRect.Right - FTextRect.Left) div FAveCharWidth);
    SB_LINEUP: Dec(FHoriOffset);
    SB_LINEDOWN: Inc(FHoriOffset);
    SB_THUMBTRACK: FHoriOffset := message.Pos;
  end;

  if FHoriOffset > SI.nMax - (FTextRect.Right - FTextRect.Left) div FAveCharWidth then
    FHoriOffset := SI.nMax - (FTextRect.Right - FTextRect.Left) div FAveCharWidth;

  if FHoriOffset < 0 then
    FHoriOffset := 0;

  if FHoriOffset = Old then
    Exit;

  SI.nPos := FHoriOffset;
  SetScrollInfo(Handle, SB_HORZ, SI, True);

  Refresh;

  // ����ʱ�����겻������������ܻᶯ
  SyncCaretPosition;
  SyncSelectionStartEnd;

  DoScroll;
end;

procedure TCnVirtualTextControl.WMKillFocus(var message: TWMSetFocus);
begin
  inherited;
  DestroyCaret;
  FCaretVisible := False;

  Invalidate;
end;

procedure TCnVirtualTextControl.WMMouseWheel(var message: TWMMouseWheel);
var
  I: Integer;
begin
  // ע�����ʱ FCaretRow �� FCaretCol ���䣬���ҿ����ܵ� TextRect ��ͷȥ
  if GetKeyState(VK_CONTROL) < 0 then
  begin
    FIsWheeling := True;
    try
      if message.WheelDelta > 0 then
        ScrollUpPage
      else
        ScrollDownPage;
    finally
      FIsWheeling := False;
    end;
  end
  else
  begin
    FIsWheeling := True;
    try
      if message.WheelDelta > 0 then
      begin
        for I := 0 to FWheelLinesCount - 1 do
          ScrollUpLine;
      end
      else
      begin
        for I := 0 to FWheelLinesCount - 1 do
          ScrollDownLine;
      end;
    finally
      FIsWheeling := False;
    end;
  end;
end;

procedure TCnVirtualTextControl.WMSetFocus(var message: TWMSetFocus);
begin
  inherited;

  if FLineHeight <= 0 then
    CalcMetrics;

  if FUseCaret then
  begin
    CreateCaret(Handle, HBITMAP(0), 2, FLineHeight);
    SetCaretBlinkTime(GetCaretBlinkTime);

    DisplayCaret(True);
  end;

  Invalidate;
end;

procedure TCnVirtualTextControl.WMSetFont(var message: TMessage);
begin
  inherited;
  Canvas.Font := Font;

  CalcMetrics;
  UpdateRects;
  UpdateScrollbars;
end;

procedure TCnVirtualTextControl.WMSize(var message: TWMSize);
begin
  inherited;
  UpdateRects;
  UpdateScrollBars;
end;

procedure TCnVirtualTextControl.WMVScroll(var message: TWMScroll);
var
  SI: TScrollInfo;
  Old, VL: Integer;
begin
  VL := VisibleLineCount;
  SI.cbSize := SizeOf(TScrollInfo);
  SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  GetScrollInfo(Handle, SB_VERT, SI);

  Old := FVertOffset;
  case message.ScrollCode of
    SB_PAGEUP: Dec(FVertOffset, VL);
    SB_PAGEDOWN: Inc(FVertOffset, VL);
    SB_LINEUP: Dec(FVertOffset);
    SB_LINEDOWN: Inc(FVertOffset);
    SB_THUMBTRACK: FVertOffset := message.Pos shl FVertExp;
  end;

  if FVertOffset > FMaxLineCount - VL then
    FVertOffset := FMaxLineCount - VL;
  if FVertOffset < 0 then
    FVertOffset := 0;

  if FVertOffset = Old then
    Exit;

  SI.nPos := FVertOffset shr FVertExp;
  SetScrollInfo(Handle, SB_VERT, SI, True);

  Refresh;

  // ����ʱ������һ�㲻��������������ᶯ
  SyncCaretPosition;
  SyncSelectionStartEnd;

  DoScroll;
end;

procedure TCnVirtualTextControl.SetUseCaret(const Value: Boolean);
begin
  if FUseCaret <> Value then
  begin
    FUseCaret := Value;
    if FUseCaret then   // ֻ����
    begin
      if HandleAllocated then
      begin
        CreateCaret(Handle, HBITMAP(0), 2, FLineHeight - 2);
        SetCaretBlinkTime(GetCaretBlinkTime);

        DisplayCaret(Focused);
      end;
    end
    else
    begin
      DisplayCaret(False);
      DestroyCaret;
    end;
  end;
end;

procedure TCnVirtualTextControl.DisplayCaret(ACaretVisible: Boolean);
begin
  if ACaretVisible and Focused then
  begin
    if HandleAllocated then
    begin
      // FCaretVisible := True;
      SyncCaretPosition;
    end;
  end
  else if not ACaretVisible then
  begin
    HideCaret(Handle);
    FCaretVisible := False;
  end;
end;

procedure TCnVirtualTextControl.UpdateCursorFrameCaret;
var
  P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  if CalcRowCol(P, FCaretRow, FCaretCol, FCharFrameIsLeft, FCharFrameDoubleWidth) then
  begin
    // ���ù��λ��
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

function TCnVirtualTextControl.ScreenColNumberToColNumber(
  ScreenColNumber: Integer): Integer;
begin
  Result := ScreenColNumber + FHoriOffset;
end;

procedure TCnVirtualTextControl.SetCaretCol(const Value: Integer);
begin
  FCaretCol := Value;

  // ���� FCaretRow �ж� FCaretCol �Ƿ񳬳���β
  // ������������������� FCaretCol
  LimitRowColumnInLine(FCaretRow, FCaretCol);

  if FUseCaret then
  begin
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

procedure TCnVirtualTextControl.SetCaretRow(const Value: Integer);
begin
  FCaretRow := Value;

  // ���� FCaretRow �ж� FCaretRow �Ƿ���� FMaxLineCount ���� FCaretCol �Ƿ񳬳���β
  // ������������������� FCaretCol
  LimitRowColumnInLine(FCaretRow, FCaretCol);

  // ͬ������ ScreenCaretCol �� ScreenCaretRow
  if FUseCaret then
  begin
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

procedure TCnVirtualTextControl.UpdateRects;
begin
  if not HandleAllocated then
    Exit;

  FTextRect.Left := GetTextRectLeft;
  FTextRect.Top := COMMON_MARGIN;
  FTextRect.Bottom := ClientRect.Bottom - COMMON_MARGIN;
  FTextRect.Right := ClientRect.Right - COMMON_MARGIN;

  FGutterRect.Top := COMMON_MARGIN;
  FGutterRect.Bottom := COMMON_MARGIN;
  if ShowLineNumber then
  begin
    FGutterRect.Left := LEFT_MARGIN;
    FGutterRect.Right := LEFT_MARGIN + FLineNumWidth + COMMON_MARGIN;
  end
  else
  begin
    FGutterRect.Left := 0;
    FGutterRect.Right := 0;
  end;
end;

function TCnVirtualTextControl.GetScreenBottomLine: Integer;
begin
  Result := GetBottomLine - GetTopLine + 1;
end;

function TCnVirtualTextControl.ColNumberToScreenColNumber(
  ColNumber: Integer): Integer;
begin
  Result := ColNumber - FHoriOffset;
end;

function TCnVirtualTextControl.LineNumberToScreenLineNumber(
  LineNumber: Integer): Integer;
begin
  Result := LineNumber - FVertOffset;
end;

procedure TCnVirtualTextControl.DoCaretChange;
begin
  if Assigned(FOnCaretChange) then
    FOnCaretChange(Self);
end;

procedure TCnVirtualTextControl.SetCaretAfterLineEnd(const Value: Boolean);
begin
  if FCaretAfterLineEnd <> Value then
  begin
    FCaretAfterLineEnd := Value;
    LimitRowColumnInLine(FCaretRow, FCaretCol);

    // ��ǰ�н�������ù��λ��
    if FUseCaret and Focused then
    begin
      SyncCaretPosition;
      SyncSelectionStartEnd;
      DoCaretChange;
    end;
    Invalidate;
  end;
end;

procedure TCnVirtualTextControl.LimitRowColumnInLine(var LineNumber, Column: Integer);
var
  C: Integer;
begin
  if LineNumber <= 0 then
    LineNumber := 1;

  if LineNumber > FMaxLineCount then
    LineNumber := FMaxLineCount;

  if Column <= 0 then
    Column := 1;

  if not CaretAfterLineEnd then
  begin
    C := GetLastColumnFromLine(LineNumber);
    if C < Column then
    begin
      Column := C;
      if Column <= 0 then
        Column := 1;
    end;
  end;
end;

procedure TCnVirtualTextControl.SetSelectEndCol(const Value: Integer);
begin
  if FSelectEndCol <> Value then
  begin
    FSelectEndCol := Value;
    LimitRowColumnInLine(FSelectEndRow, FSelectEndCol);
    SetCaretRowCol(FSelectEndRow, FSelectEndCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.SetSelectEndRow(const Value: Integer);
begin
  if FSelectEndRow <> Value then
  begin
    FSelectEndRow := Value;
    LimitRowColumnInLine(FSelectEndRow, FSelectEndCol);
    SetCaretRowCol(FSelectEndRow, FSelectEndCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.SetSelectStartCol(const Value: Integer);
begin
  if FSelectStartCol <> Value then
  begin
    FSelectStartCol := Value;
    LimitRowColumnInLine(FSelectStartRow, FSelectStartCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.SetSelectStartRow(const Value: Integer);
begin
  if FSelectStartRow <> Value then
  begin
    FSelectStartRow := Value;
    LimitRowColumnInLine(FSelectStartRow, FSelectStartCol);
    Invalidate;
    DoSelectChange;
  end;
end;

procedure TCnVirtualTextControl.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
  TR: TRect;
begin
  inherited;

  if FLeftMouseDown then
  begin
    P.x := X;
    P.y := Y;

    // �϶����˱�Ե���ȹ���һ���������� SetCapture
    TR := GetTextRect;
    if P.x < TR.Left then
      ScrollLeftCol
    else if P.x > TR.Right then
      ScrollRightCol;

    if P.y < TR.Top then
      ScrollUpLine
    else if P.y > TR.Bottom then
      ScrollDownLine;

    if FUseSelection then     // ����϶�ʱ���֧��ѡ�����������ѡ����β
    begin
      if not FLeftMouseMoveAfterDown then // �����϶��� MouseDown ����״� Move
      begin
        // TODO: �ж��Ƿ���ѡ�������ٴ��϶��������������ק�����ڼ򵥵�ȡ��ѡ��׼���ٴ�ѡ��
        SyncSelectionStartEnd(True);
      end;

      FLeftMouseMoveAfterDown := True;

      CalcSelectEnd(P); // Ȼ��������������ε�ѡ����
      SetCaretRowCol(FSelectEndRow, FSelectEndCol); // ͬʱҲ�ƶ����
    end
    else
    begin
      UpdateCursorFrameCaret; // ����϶�ʱ�����֧��ѡ��������Ҳ�ƶ����
      ScrollToVisibleCaret;
    end;
  end;
end;

procedure TCnVirtualTextControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button = mbLeft then
  begin
    FLeftMouseDown := False;
    if FUseSelection then  // ������̧��ʱ��Ҫͨ��ʲô����ȡ����ǰѡ����������δ�϶�
    begin
      if not FLeftMouseMoveAfterDown and HasSelection then // �϶�����ɶ��������δ�϶�����ȡ��ѡ��
      begin
        SyncSelectionStartEnd(True);
        Invalidate;
        DoSelectChange;
      end;
    end;
  end;
end;

procedure TCnVirtualTextControl.SetCaretRowCol(Row, Col: Integer);
begin
  FCaretRow := Row;
  FCaretCol := Col;

  // ���� FCaretRow �ж� FCaretCol �Ƿ񳬳���β
  // ������������������� FCaretCol
  LimitRowColumnInLine(FCaretRow, FCaretCol);

  if FUseCaret then
  begin
    SyncCaretPosition;
    SyncSelectionStartEnd;
    DoCaretChange;
  end;
end;

procedure TCnVirtualTextControl.ScrollToVisibleCaret;
var
  M: Boolean;
  L: Integer;
begin
  if not HandleAllocated and not FUseCaret then
    Exit;

  M := False;
  if FCaretRow < GetTopLine then
  begin
    Dec(FVertOffset, GetTopLine - FCaretRow);
    M := True;
  end
  else if FCaretRow > GetBottomLine then
  begin
    Inc(FVertOffset, FCaretRow - GetBottomLine);
    M := True;
  end;

  // �ǵȿ��У�����ֱ�ӱȽ��С����ȼ������ǰ Column ��Ӧ���������������࣬���ѹ���������Ƚ�
  L := GetColumnVirtualX(FCaretRow, FCaretCol);
  if GetHoriPixelsOffset > L - CARET_MARGIN then
  begin
    // ����ѹ������������˵����ǰ Column ��������࣬��Ҫ�������ൽ�պñ� L ��Ӧ���ַ����Сһ���
    FHoriOffset := L div FAveCharWidth;
    M := True;
  end
  else if GetHoriPixelsOffset + (FTextRect.Right - FTextRect.Left) < L + CARET_MARGIN then
  begin
    // ����ѹ�����������ı�����ȣ�Ҳ�����ѹ������Ҳ�ࣩ̫С��˵����ǰ Column �������Ҳ࣬
    // ��Ҫ�������ൽ�Ҳ���Ӧ���ַ���ȴ�һ���
    FHoriOffset := ((L - (FTextRect.Right - FTextRect.Left)) div FAveCharWidth) + 1;
    M := True;
  end;

  if M then
  begin
    Invalidate;
    SyncCaretPosition;
    UpdateScrollBars;
    DoScroll;
  end;
end;

procedure TCnVirtualTextControl.SetUseSelection(const Value: Boolean);
begin
  if FUseSelection <> Value then
  begin
    FUseSelection := Value;
    SyncSelectionStartEnd(True);
    Invalidate;
  end;
end;

function TCnVirtualTextControl.HasSelection: Boolean;
begin
  Result := (FSelectStartRow <> FSelectEndRow) or (FSelectStartCol <> FSelectEndCol);
end;

function TCnVirtualTextControl.SyncSelectionStartEnd(Force: Boolean): Boolean;
begin
  if FUseSelection and (Force or not HasSelection) then
  begin
    FSelectStartRow := FCaretRow;
    FSelectEndRow := FCaretRow;
    FSelectStartCol := FCaretCol;
    FSelectEndCol := FCaretCol;

    Result := True;
  end
  else
    Result := False;
end;

function TCnVirtualTextControl.CalcRowCol(Pt: TPoint; out ACaretRow, ACaretCol: Integer;
  out ACharFrameIsLeft, ACharFrameDoubleWidth: Boolean): Boolean;
begin
  Result := ClientPosToVirtualCharPos(Pt, ACaretRow, ACaretCol, ACharFrameIsLeft,
    ACharFrameDoubleWidth);

  if Result then
  begin
    // ͨ������ Row/Col �ж�����
    LimitRowColumnInLine(ACaretRow, ACaretCol);

    if not ACharFrameIsLeft then
      ACaretCol := GetNextColumn(ACaretCol, ACaretRow, CaretAfterLineEnd);

    // ͨ������ Row/Col �ж�����
    LimitRowColumnInLine(ACaretRow, ACaretCol);
  end;
end;

procedure TCnVirtualTextControl.DoSelectChange;
begin
  if Assigned(FOnSelectChange) then
    FOnSelectChange(Self);
end;

procedure TCnVirtualTextControl.CalcSelectEnd(Pt: TPoint);
var
  ACaretRow, ACaretCol: Integer;
  OldSelEndRow, OldSelEndCol: Integer;
  ACharFrameIsLeft, ACharFrameDoubleWidth: Boolean;
begin
  // �����϶�ѡ��ע�� Down ʱ�Ѿ�ȷ������ѡ����ʼ��
  OldSelEndRow := FSelectEndRow;
  OldSelEndCol := FSelectEndCol;

  if CalcRowCol(Pt, ACaretRow, ACaretCol, ACharFrameIsLeft, ACharFrameDoubleWidth) then
  begin
    // ������������ڻ��������������к���
    FSelectEndRow := ACaretRow;
    FSelectEndCol := ACaretCol;

    LimitRowColumnInLine(FSelectEndRow, FSelectEndCol); // ���Ʊ����ϳ���Χ

    if (FSelectEndRow <> OldSelEndRow) or (FSelectEndCol <> OldSelEndCol) then
    begin
      Invalidate;
      DoSelectChange;
    end;
  end;
end;

procedure TCnVirtualTextControl.SetOnSelectChange(
  const Value: TNotifyEvent);
begin
  FOnSelectChange := Value;
end;

procedure TCnVirtualTextControl.ScrollDownLine;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINEDOWN;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollDownPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGEDOWN;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollLeftCol;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINELEFT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollLeftPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGELEFT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollRightCol;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINERIGHT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollRightPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGERIGHT;
  WMHScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollUpLine;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_LINEUP;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.ScrollUpPage;
var
  Msg: TWMScroll;
begin
  Msg.ScrollCode := SB_PAGEUP;
  WMVScroll(Msg);
end;

procedure TCnVirtualTextControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  UpdateRects;
  UpdateScrollBars;
end;

function TCnVirtualTextControl.GetNextColumn(AColumn,
  ARow: Integer; ACaretAfterLineEnd: Boolean): Integer;
begin
  Result := AColumn + 1;
end;

function TCnVirtualTextControl.GetPrevColumn(AColumn,
  ARow: Integer): Integer;
begin
  Result := AColumn - 1;
end;

function TCnVirtualTextControl.GetHoriPixelsOffset: Integer;
begin
  Result := FHoriOffset * FAveCharWidth;
end;

function TCnVirtualTextControl.CalcPixelOffsetFromColumnInLine(
  ARow, ACol: Integer; out Rect: TRect; out DoubleWidth: Boolean): Boolean;
begin
  Rect.Top := 0;
  Rect.Left := FAveCharWidth * (ACol - 1);
  Rect.Right := Rect.Left + FAveCharWidth;
  Rect.Bottom := FLineHeight;

  DoubleWidth := False;
  Result := True;
end;

function TCnVirtualTextControl.GetScreenCaretRow: Integer;
begin
  Result := LineNumberToScreenLineNumber(FCaretRow);
end;

function TCnVirtualTextControl.GetColumnVirtualX(ARow,
  ACol: Integer): Integer;
var
  R: TRect;
begin
  GetVirtualCharPosVirtualRect(ARow, ACol, R);
  Result := R.Left;
end;

function TCnVirtualTextControl.ClientXToVirtualX(X: Integer): Integer;
begin
  // ��������꣬��ȥ�����������������꣬���Ǹõ�������������߾࣬���Ϻ����������
  Result := X - GetTextRect.Left + GetHoriPixelsOffset;
end;

function TCnVirtualTextControl.VirtualXToClientX(X: Integer): Integer;
begin
  Result := X - GetHoriPixelsOffset + GetTextRect.Left
end;

procedure TCnVirtualTextControl.SyncCaretPosition;
var
  R: TRect;
begin
  if FUseCaret then
  begin
    if not GetVirtualCharPosPhysicalRect(FCaretRow, FCaretCol, R) then
      Exit;

    if (R.Left >= FTextRect.Left) and (R.Left <= FTextRect.Right) then
    begin
      if not FCaretVisible then
      begin
        ShowCaret(Handle);
        FCaretVisible := True;
      end;
      SetCaretPos(R.Left, R.Top);
    end
    else
    begin
      if FCaretVisible then
      begin
        HideCaret(Handle);
        FCaretVisible := False;
      end;
    end;
  end;
end;

procedure TCnVirtualTextControl.DoPaintLineBackground(LineCanvas: TCanvas;
  LineNumber: Integer; LineRect: TRect);
begin
  LineCanvas.Brush.Color := Color;
  LineCanvas.Brush.Style := bsSolid;
  LineCanvas.FillRect(LineRect);
end;

function TCnVirtualTextControl.GetNearestColumn(AColumn,
  ARow: Integer): Integer;
begin
  Result := AColumn;
end;

procedure TCnVirtualTextControl.MakeOrderSelection(var SelStartRow,
  SelStartCol, SelEndRow, SelEndCol: Integer);
var
  T: Integer;
begin
  if (SelEndRow < SelStartRow) or ((SelEndRow = SelStartRow) and (SelEndCol < SelStartCol)) then
  begin
    T := SelEndRow;
    SelEndRow := SelStartRow;
    SelStartRow := T;

    T := SelEndCol;
    SelEndCol := SelStartCol;
    SelStartCol := T;
  end;    // ȷ�� StartRow/Col �� EndRow/Col ǰ��
end;

procedure TCnVirtualTextControl.SelectRange(StartRow, StartCol, EndRow,
  EndCol: Integer);
begin
  if not FUseSelection then
    Exit;

  MakeOrderSelection(StartRow, StartCol, EndRow, EndCol);
  FSelectStartRow := StartRow;
  FSelectStartCol := StartCol;
  FSelectEndRow := EndRow;
  FSelectEndCol := EndCol;
  LimitRowColumnInLine(FSelectStartRow, FSelectStartCol);
  LimitRowColumnInLine(FSelectEndRow, FSelectEndCol);
  SetCaretRowCol(FSelectEndRow, FSelectEndCol);
  SyncSelectionStartEnd;
  Invalidate;
  DoSelectChange;
end;

procedure TCnVirtualTextControl.SelectAll;
begin
  SelectRange(1, 1, MaxInt, MaxInt);
end;

end.
