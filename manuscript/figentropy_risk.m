clf;
% automatically create postscript whenever
% figure is drawn
tmpfilename = 'figentropy_risk';
tmpfilebwname = sprintf('%s_noname_bw',tmpfilename);
tmpfilenoname = sprintf('%s_noname',tmpfilename);

tmpprintname = fixunderbar(tmpfilename);
% for use with xfig and pstex
tmpxfigfilename = sprintf('x%s',tmpfilename);

tmppos= [0.2 0.2 0.7 0.7];
tmpa1 = axes('position',tmppos);

set(gcf,'DefaultLineMarkerSize',10);
% set(gcf,'DefaultLineMarkerEdgeColor','k');
% set(gcf,'DefaultLineMarkerFaceColor','w');
set(gcf,'DefaultAxesLineWidth',2);

set(gcf,'PaperPositionMode','auto');

% main data goes here

x=csvread('entropy_all_5_10.csv');
eventsize=x(:,1);
H_5 = x(:,2);
H_10 = x(:,3);
tmph=semilogx(eventsize,H_5,'k-');
set(tmph,'linewidth',3,'color',[0.5 0.5 0.5]);
hold on
tmph=semilogx(eventsize,H_10,'k-');
set(tmph,'linewidth',3,'color','k');
[yval tmpi]=max(H_10);
hold on

% Annotations
[yval tmpi]=max(H_5);
tmph=plot(eventsize(tmpi),yval,'ko');
set(tmph,'markerfacecolor',[0.5 0.5 0.5],'markersize',10);
[yval tmpi]=max(H_10);
tmph=plot(eventsize(tmpi),yval,'ko');
set(tmph,'markerfacecolor','k','markersize',10);

% loglog(,, '');
%
%
% Some helpful plot commands
% tmph=plot(x,y,'ko');
% set(tmph,'markersize',10,'markerfacecolor,'k');
% tmph=plot(x,y,'k-');
% set(tmph,'linewidth',2);

set(gca,'fontsize',20);

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
set(gca,'xtick',[10 25 50 100 250 500 1000]);
% set(gca,'ytick',[1 100 10^4])

% creation of postscript for papers
% psprint(tmpxfigfilename);

% the following will usually not be printed 
% in good copy for papers
% (except for legend without labels)

% legend
% tmplh = legend('stuff',...);
tmplh = legend('5x','10x');
set(tmplh,'fontsize',18);
% remove box
% set(tmplh,'visible','off')
legend('boxoff');

xlabel('Event size','fontsize',20,'verticalalignment','top','interpreter','latex');
ylabel('Visualization entropy','fontsize',20,'verticalalignment','bottom','interpreter','latex');
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
