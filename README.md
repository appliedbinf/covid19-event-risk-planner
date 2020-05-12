# COVID19 risk planner R-Shiny application



## Installation

`COVID19 risk planner R-Shiny application` is an R-Shiny application that requires R 3.4+, shinyserver, and several R packages.  Optionally, you can deploy behind a webserver (Apache or NGINX) to act as the reverse proxy and handle SSL termination.

**From Github**
```bash
git clone git@github.com:appliedbinf/covid19-event-risk-planner.git 
sudo mv covid19-event-risk-planner/COVID19-Event-Risk-Planner /srv/shinyserver/COVID19-Event-Risk-Planner
# Install R and Shiny Server with your distro package manager
# See https://rstudio.com/products/shiny/download-server/ for
# Shiny Server information

Rscript -e "install.packages(c( 'shiny', 'shinythemes', \
'ggplot2', 'ggthemes', 'ggpubr', 'ggrepel', 'dplyr', \
'lubridate', 'matlab'))"

```

### Requirements
* wget
* coreutils
* R 3.4+
* Shiny
* shinythemes
* ggplot2
* ggthemes
* ggpubr
* ggrepel
* dplyr
* lubridate
* matlab (R package)


### Crontab entries
We update the current data for the application every hour (with a random delay between 0-700 seconds) and the daily data every 4 hours.  While we could load the data live from the API, we want the application to be functional and responsive even if the API is down/slow and it ensures we're not hammering the API if we get a spike of users.  The real time daily data doesn't change often enough for this to introduce major delays in reporting.  

```
1 * * * * perl -le 'sleep rand 700' && /srv/shinyserver/COVID19-Event-Risk-Planner/update_current.sh
1 */4 * * * /srv/shinyserver/COVID19-Event-Risk-Planner/update_daily.sh

```
### Example Apache config
```
<VirtualHost *:80>

ServerName example.com
Redirect permanent / https://example.com/

</VirtualHost>

<VirtualHost *:443>
ServerName example.com
ErrorLog /var/logs/covid19risk/logs/error_log

SSLEngine on
SSLCertificateFile /etc/httpd/ssl/example.com_cert.crt
SSLCertificateKeyFile /etc/httpd/ssl/example.com.key
SSLProxyEngine On
SSLProtocol all -SSLv2 -SSLv3 -TLSv1 -TLSv1.1
SSLCipherSuite HIGH:!aNULL:!MD5:!3DES
SSLHonorCipherOrder on

ProxyPreserveHost On
ProxyPass / localhost:3432/
ProxyPassReverse / localhost:3432/

</VirtualHost>

```
