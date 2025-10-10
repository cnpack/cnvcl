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

unit CnConsts;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�������Դ�ַ������嵥Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2004.09.18 V1.2
*                ����CnMemProf���ַ�������
*           2002.04.18 V1.1
*                ���������ַ�������
*           2002.04.08 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

const
  ECN_OK                              = 0;         // ������ OK���޴���

  ECN_FILE_NOT_FOUND                  = $10;       // �ļ�������

  ECN_CUSTOM_ERROR_BASE               = $1000;     // ������趨�Ĵ�������ʼֵ

//==============================================================================
// ����Ҫ���ػ����ַ���
//==============================================================================

resourcestring

  // ע���·��
  SCnPackRegPath = '\Software\CnPack';

  // ��������·��
  SCnPackToolRegPath = 'CnTools';

//==============================================================================
// ��Ҫ���ػ����ַ���
//==============================================================================


var
  // ������Ϣ
  SCnHint: string = '��ʾ';
  SCnInformation: string = '��Ϣ';
  SCnWarning: string = '����';
  SCnError: string = '����';
  SCnEnabled: string = '��Ч';
  SCnDisabled: string = '����';
  SCnMsgDlgOK: string = 'ȷ��(&O)';
  SCnMsgDlgCancel: string = 'ȡ��(&C)';
  SCnMsgDlgYes: string = '��(&Y)';
  SCnMsgDlgNo: string = '��(&N)';
  SCnMsgDlgYesToAll: string = 'ȫ��(&A)';
  SCnMsgDlgNoToAll: string = 'ȫ��(&l)';
  SCnVersion: string = '�汾';
  SCnNeedAdmin: string = '������Ҫ����ԱȨ��';
  SCnNotSupport: string = '�ò�����֧��';

const
  // ��������Ϣ
  SCnPackAbout = 'CnPack';
  SCnPackVer = 'Ver 0.1.7.0';
  SCnPackStr = SCnPackAbout + ' ' + SCnPackVer;
  SCnPackUrl = 'https://www.cnpack.org';
  SCnPackBbsUrl = 'https://bbs.cnpack.org';
  SCnPackNewsUrl = 'news://news.cnpack.org';
  SCnPackSourceUrl = 'https://github.com/cnpack';
  SCnPackEmail = 'master@cnpack.org';
  SCnPackBugEmail = 'bugs@cnpack.org';
  SCnPackSuggestionsEmail = 'suggestions@cnpack.org';

  SCnPackDonationUrl = 'https://www.cnpack.org/donation.php';
  SCnPackDonationUrlSF = 'http://sourceforge.net/donate/index.php?group_id=110999';
  SCnPackGroup = 'CnPack ������';
  SCnPackCopyright = '(C)Copyright 2001-2025 ' + SCnPackGroup;

  // CnPropEditors
  SCopyrightFmtStr =
    SCnPackStr + #13#10#13#10 +
    '�������: %s' + #13#10 +
    '�������: %s(%s)' + #13#10 +
    '���˵��: %s' + #13#10#13#10 +
    '������վ: ' + SCnPackUrl + #13#10 +
    '����֧��: ' + SCnPackEmail + #13#10#13#10 +
    SCnPackCopyright;

resourcestring

  // �����װ�����
  SCnNonVisualPalette = 'CnPack Tools';
  SCnGraphicPalette = 'CnPack VCL';
  SCnNetPalette = 'CnPack Net';
  SCnDatabasePalette = 'CnPack DB';
  SCnReportPalette = 'CnPack Report';

  // �������Ա��Ϣ���ں�����ӣ�ע�Ȿ�ػ�����
var
  SCnPack_Team: string = 'CnPack������';
  SCnPack_Zjy: string = '�ܾ���';
  SCnPack_Shenloqi: string = '����ǿ(Chinbo)';
  SCnPack_xiaolv: string = '������';
  SCnPack_Flier: string = 'Flier Lu';
  SCnPack_LiuXiao: string = 'CnPack ������(Passion)';
  SCnPack_PanYing: string = '��ӥ(Pan Ying)';
  SCnPack_Hubdog: string = '��ʡ(Hubdog)';
  SCnPack_Wyb_star: string = '����';
  SCnPack_Licwing: string = '����(Licwing Zue)';
  SCnPack_Alan: string = '��ΰ(Alan)';
  SCnPack_GuYueChunQiu: string = '���´���';
  SCnPack_Aimingoo: string = '�ܰ���(Aimingoo)';
  SCnPack_QSoft: string = '����(QSoft)';
  SCnPack_Hospitality: string = '������(Hospitality)';
  SCnPack_SQuall: string = '����(SQUALL)';
  SCnPack_Hhha: string = 'Hhha';
  SCnPack_Beta: string = '�ܺ�(beta)';
  SCnPack_Leeon: string = '���(Leeon)';
  SCnPack_SuperYoyoNc: string = '���ӽ�';
  SCnPack_JohnsonZhong: string = 'Johnson Zhong';
  SCnPack_DragonPC: string = 'Dragon P.C.';
  SCnPack_Kendling: string = 'С��(Kending)';
  SCnPack_ccrun: string = 'ccRun(����)';
  SCnPack_Dingbaosheng: string = 'dingbaosheng';
  SCnPack_LuXiaoban: string = '���沨(³С��)';
  SCnPack_Savetime: string = 'savetime';
  SCnPack_solokey: string = 'solokey';
  SCnPack_Bahamut: string = '�͹�ķ��';
  SCnPack_Sesame: string = '������(Sesame)';
  SCnPack_BuDeXian: string = '������';
  SCnPack_XiaoXia: string = 'С��';
  SCnPack_ZiMin: string = '�ӕF';
  SCnPack_rarnu: string = 'rarnu';
  SCnPack_dejoy: string = 'dejoy';
  SCnPack_Rain: string = 'Rain';
  SCnPack_cnwinds: string = 'cnwinds';

  // CnCommon
  SUnknowError: string = 'δ֪����';
  SErrorCode: string = '������룺';

const
  SCnPack_TeamEmail = 'master@cnpack.org';
  SCnPack_ZjyEmail = 'zjy@cnpack.org';
  SCnPack_ShenloqiEmail = 'Shenloqi@hotmail.com';
  SCnPack_xiaolvEmail = 'xiaolv888@etang.com';
  SCnPack_FlierEmail = 'flier_lu@sina.com';
  SCnPack_LiuXiaoEmail = 'liuxiao@cnpack.org';
  SCnPack_PanYingEmail = 'panying@sina.com';
  SCnPack_HubdogEmail = 'hubdog@263.net';
  SCnPack_Wyb_starMail = 'wyb_star@sina.com';
  SCnPack_LicwingEmail = 'licwing@chinasystemsn.com';
  SCnPack_AlanEmail = 'BeyondStudio@163.com';
  SCnPack_GuYueChunQiuEmail = 'guyuechunqiu@cnpack.org';
  SCnPack_AimingooEmail = 'aim@263.net';
  SCnPack_QSoftEmail = 'hq.com@263.net';
  SCnPack_HospitalityEmail = 'Hospitality_ZJX@msn.com';
  SCnPack_SQuallEmail = 'squall_sa@163.com';
  SCnPack_HhhaEmail = 'Hhha@eyou.com';
  SCnPack_BetaEmail = 'beta@01cn.net';
  SCnPack_LeeonEmail = 'real-like@163.com';
  SCnPack_SuperYoyoNcEmail = 'superyoyonc@sohu.com';
  SCnPack_JohnsonZhongEmail = 'zhongs@tom.com';
  SCnPack_DragonPCEmail = 'dragonpc@21cn.com';
  SCnPack_KendlingEmail = 'kendling@21cn.com';
  SCnPack_ccRunEmail = 'info@ccrun.com';
  SCnPack_DingbaoshengEmail = 'yzdbs@msn.com';
  SCnPack_LuXiaobanEmail = 'zhouyibo2000@sina.com';
  SCnPack_SavetimeEmail = 'savetime2k@hotmail.com';
  SCnPack_solokeyEmail = 'crh611@163.com';
  SCnPack_BahamutEmail = 'fantasyfinal@126.com';
  SCnPack_SesameEmail = 'sesamehch@163.com';
  SCnPack_BuDeXianEmail = 'appleak46@yahoo.com.cn';
  SCnPack_XiaoXiaEmail = 'summercore@163.com';
  SCnPack_ZiMinEmail = '441414288@qq.com';
  SCnPack_rarnuEmail = 'rarnu@cnpack.org';
  SCnPack_dejoyEmail = 'dejoybbs@163.com';
  SCnPack_RainEmail = SCnPack_TeamEmail;
  SCnPack_cnwindsEmail = SCnPack_TeamEmail;

  // CnMemProf
  SCnPackMemMgr = '�ڴ���������';
  SMemLeakDlgReport = '���� %d ���ڴ�©��[�滻�ڴ������֮ǰ�ѷ��� %d ��]��';
  SMemMgrODSReport = '��ȡ = %d���ͷ� = %d���ط��� = %d';
  SMemMgrOverflow = '�ڴ���������ָ���б�������������б�������';
  SMemMgrRunTime = '%d Сʱ %d �� %d �롣';
  SOldAllocMemCount = '�滻�ڴ������ǰ�ѷ��� %d ���ڴ档';
  SAppRunTime = '��������ʱ��: ';
  SMemSpaceCanUse = '���õ�ַ�ռ�: %d ǧ�ֽ�';
  SUncommittedSpace = 'δ�ύ����: %d ǧ�ֽ�';
  SCommittedSpace = '���ύ����: %d ǧ�ֽ�';
  SFreeSpace = '���в���: %d ǧ�ֽ�';
  SAllocatedSpace = '�ѷ��䲿��: %d ǧ�ֽ�';
  SAllocatedSpacePercent = '��ַ�ռ�����: %d%%';
  SFreeSmallSpace = 'ȫ��С�����ڴ��: %d ǧ�ֽ�';
  SFreeBigSpace = 'ȫ��������ڴ��: %d ǧ�ֽ�';
  SUnusedSpace = '����δ���ڴ��: %d ǧ�ֽ�';
  SOverheadSpace = '�ڴ����������: %d ǧ�ֽ�';
  SObjectCountInMemory = '�ڴ������Ŀ: ';
  SNoMemLeak = 'û���ڴ�й©��';
  SNoName = '(δ����)';
  SNotAnObject = '���Ƕ���';
  SByte = '�ֽ�';
  SCommaString = '��';
  SPeriodString = '��';

resourcestring
  SCnErrorMapViewOfFile = 'MapViewOfFile ʧ�� ';
  SCnErrorCreateFileMapping = 'CreateFileMapping ʧ�� ';

function CnGetLastError: Integer;

procedure _CnSetLastError(Err: Integer);

implementation

threadvar
  CnErrorCode: Integer;

function CnGetLastError: Integer;
begin
  Result := CnErrorCode;
end;

procedure _CnSetLastError(Err: Integer);
begin
  CnErrorCode := Err;
end;

end.

