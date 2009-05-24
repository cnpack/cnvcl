create database [MainDetailDemo]
go

use [MainDetailDemo]
go

create table [mainTable](
  [ID] varchar(20) primary key,
  [NAME] varchar(20)
)
go

create table [DetailTable](
  [ID] varchar(20) primary key,  
  [MainID] varchar(20) not null,
  [WORK] varchar(200)
)
go

create table [Idg] (
  [CODE] varchar(20) primary key,
  [NAME] varchar(20)
)
GO

insert into [mainTable] values('1','aaa')
insert into [mainTable] values('2','bbb')
insert into [mainTable] values('3','ccc')

insert into [DetailTable] values ('1', '1', '123')
insert into [DetailTable] values ('2', '1', '456')
insert into [DetailTable] values ('3', '1', '789')

insert into [DetailTable] values ('4', '2', 'abc')
insert into [DetailTable] values ('5', '2', 'def')
insert into [DetailTable] values ('6', '2', 'ghi')

insert into [DetailTable] values ('7', '3', 'xxx')
insert into [DetailTable] values ('8', '3', 'yyy')
insert into [DetailTable] values ('9', '3', 'zzz')
