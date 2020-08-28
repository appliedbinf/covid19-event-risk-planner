clf;
% automatically create postscript whenever
% figure is drawn
tmpfilename = 'fig100index_v2';
tmpfilebwname = sprintf('%s_noname_bw',tmpfilename);
tmpfilenoname = sprintf('%s_noname',tmpfilename);

tmpprintname = fixunderbar(tmpfilename);
% for use with xfig and pstex
tmpxfigfilename = sprintf('x%s',tmpfilename);

set(gcf,'DefaultLineMarkerSize',10);
% set(gcf,'DefaultLineMarkerEdgeColor','k');
% set(gcf,'DefaultLineMarkerFaceColor','w');
set(gcf,'DefaultAxesLineWidth',2);

set(gcf,'PaperPositionMode','auto');
set(gcf,'Position',[505 43 772 912]);

% main data goes here
[x5,txt,raw]=xlsread('p_vals_only5.xlsx');
[x10,txt,raw]=xlsread('p_vals_only10.xlsx');
[tmpend,y]=sort(x5(end,2:53),'ascend');
for i=1:52,
  tmprow = floor((i-1)/4);
  tmpcol = rem(i-1,4);
  tmppos= [0.1+(tmpcol)*0.2 0.85-0.07*(tmprow-1) 0.2 0.07];
  tmpa1 = axes('position',tmppos);
  pval= x5(:,1+y(i));
  n50_5 = 1-(1-pval).^50;
  tmph=plot(n50_5);
  set(tmph,'linewidth',5);
  hold on
  pval= x10(:,1+y(i));
  n50_10 = 1-(1-pval).^50;
  tmph=plot(n50_10);
  set(tmph,'linewidth',5,'color',[0 0.75 0.75]);
  ylim([0 1]);
  hold on
  plot([1 length(pval)],[0.5 0.5],'k--');
  set(gca,'xtick',[]);
  set(gca,'ytick',[]);
  tmps=sprintf('(%d) %s',i,txt{1+y(i)});
  tmps2=text(1,0.8,tmps);
  set(tmps2,'fontsize',16);
  if (tmpcol==0)
    set(gca,'ytick',[0.25:0.25:0.75]);
    set(gca,'fontsize',16);
    set(gca,'yticklabel',{'25%';'50%';'75%'});
  end
  if (tmpcol==3)
    set(gca,'ytick',[0.25:0.25:0.75]);
    set(gca,'fontsize',16);
    set(gca,'yaxislocation','right');
    set(gca,'yticklabel',{'25%';'50%';'75%'});
  end
  if (tmprow==12)
    set(gca,'xtick',[1 32 62 93]);
    set(gca,'fontsize',16);
    set(gca,'xticklabel',{'5/1/20';'6/1/20';'7/1/20';'8/1/20'});
    set(gca,'xticklabelrotation',-45);
  end
  if (tmprow==6 & tmpcol==0)
    ylabel('Event-associated risk','fontsize',20,'verticalalignment','bottom','interpreter','latex');
  elseif (tmprow==6 & tmpcol==3)
    ylabel('Event-associated risk','fontsize',20,'verticalalignment','top','interpreter','latex');
  else
    xlabel('','fontsize',20,'verticalalignment','top','interpreter','latex');
    ylabel('','fontsize',20,'verticalalignment','bottom','interpreter','latex');
  end
end
% loglog(,, '');
%
%
% Some helpful plot commands
% tmph=plot(x,y,'ko');
% set(tmph,'markersize',10,'markerfacecolor,'k');
% tmph=plot(x,y,'k-');
% set(tmph,'linewidth',2);


% for use with layered plots
% set(gca,'box','off')

% adjust limits
% tmpv = axis;
% axis([]);
% ylim([]);
% xlim([]);

% change axis line width (default is 0.5)
% set(tmpa1,'linewidth',2)

% fix up tickmarks
% set(gca,'xtick',[1 100 10^4])
% set(gca,'ytick',[1 100 10^4])

% creation of postscript for papers
% psprint(tmpxfigfilename);

% the following will usually not be printed 
% in good copy for papers
% (except for legend without labels)

% legend
% tmplh = legend('stuff',...);
% tmplh = legend('','','');
% remove box
% set(tmplh,'visible','off')
% legend('boxoff');

% title('','fontsize',24)
% 'horizontalalignment','left');

% for writing over the top
% coordinates are normalized again to (0,1.0)
tmpa2 = axes('Position', tmppos);
set(tmpa2,'visible','off');
% first two points are normalized x, y positions
% text(,,'','Fontsize',14);

% automatic creation of postscript
% without name/date
psprintc(tmpfilenoname);
psprint(tmpfilebwname);

tmpt = pwd;
tmpnamememo = sprintf('[source=%s/%s.ps]',tmpt,tmpprintname);
text(1.05,.05,tmpnamememo,'Fontsize',6,'rotation',90);
datenamer(1.1,.05,90);
% datename(.5,.05);
% datename2(.5,.05); % 2 rows

% automatic creation of postscript
psprintc(tmpfilename);

% set following on if zooming of 
% plots is required
% may need to get legend up as well
%axes(tmpa1)
%axes(tmplh)
clear tmp*
