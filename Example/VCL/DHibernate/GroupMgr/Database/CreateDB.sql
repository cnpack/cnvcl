CREATE DATABASE [QQGroup]
GO

USE [QQGroup]
GO

CREATE TABLE [Constants] (
	[Code] [varchar] (20) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[Name] [varchar] (20) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	 PRIMARY KEY  CLUSTERED 
	(
		[Code]
	)  ON [PRIMARY] 
) ON [PRIMARY]
GO

CREATE TABLE [Idg] (
	[CODE] [varchar] (20) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[NAME] [varchar] (20) COLLATE Chinese_PRC_CI_AS NULL ,
	 PRIMARY KEY  CLUSTERED 
	(
		[CODE]
	)  ON [PRIMARY] 
) ON [PRIMARY]
GO

CREATE TABLE [Members] (
	[QQCode] [varchar] (20) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[UserName] [varchar] (32) COLLATE Chinese_PRC_CI_AS NULL ,
	[Sex] [char] (1) COLLATE Chinese_PRC_CI_AS NULL ,
	[Age] [varchar] (10) COLLATE Chinese_PRC_CI_AS NULL ,
	[Area] [varchar] (100) COLLATE Chinese_PRC_CI_AS NULL ,
	[NameCard] [varchar] (32) COLLATE Chinese_PRC_CI_AS NULL ,
	[Email] [varchar] (200) COLLATE Chinese_PRC_CI_AS NULL ,
	[WebSite] [varchar] (200) COLLATE Chinese_PRC_CI_AS NULL ,
	[Research] [ntext] COLLATE Chinese_PRC_CI_AS NULL ,
	[Status] [varchar] (12) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[OutReason] [ntext] COLLATE Chinese_PRC_CI_AS NULL ,
	[InTime] [datetime] NULL ,
	[OutTime] [datetime] NULL ,
	[Identity] [varchar] (12) COLLATE Chinese_PRC_CI_AS NULL ,
	[GotWarn] [char] (1) COLLATE Chinese_PRC_CI_AS NULL ,
	[GotResearch] [char] (1) COLLATE Chinese_PRC_CI_AS NULL ,
	CONSTRAINT [PK__Members__76CBA758] PRIMARY KEY  CLUSTERED 
	(
		[QQCode]
	)  ON [PRIMARY] 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [Researchs] (
	[SelfId] [varchar] (18) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[QQCode] [varchar] (20) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[Time] [datetime] NULL ,
	[Context] [ntext] COLLATE Chinese_PRC_CI_AS NULL ,
	 PRIMARY KEY  CLUSTERED 
	(
		[SelfId]
	)  ON [PRIMARY] 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE TABLE [Warnings] (
	[SelfId] [varchar] (18) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[QQCode] [varchar] (20) COLLATE Chinese_PRC_CI_AS NOT NULL ,
	[WarnTime] [datetime] NOT NULL ,
	[Reason] [ntext] COLLATE Chinese_PRC_CI_AS NULL ,
	 PRIMARY KEY  CLUSTERED 
	(
		[SelfId]
	)  ON [PRIMARY] 
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

INSERT INTO [Constants] VALUES('M', '男')
INSERT INTO [Constants] VALUES('F', '女')
INSERT INTO [Constants] VALUES('In', '在群')
INSERT INTO [Constants] VALUES('Out', '已退群')
INSERT INTO [Constants] VALUES('Admin', '管理员')
INSERT INTO [Constants] VALUES('Member', '会员')
INSERT INTO [Constants] VALUES('Y', '是')
INSERT INTO [Constants] VALUES('N', '否')
