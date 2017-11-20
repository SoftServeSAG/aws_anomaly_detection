# Anomaly detection demo for AWS

This repository contains code from the old anomaly detection project, transformed into R Shiny application with remote (AWS) deployment support.

## Docker install ##

1. Get application source code:
```bash
git clone https://github.com/SoftServeSAG/aws_anomaly_detection
cd aws_anomaly_detection/
```

2. Build docker image:
```bash
docker build --rm --force-rm -t anomaly_detection .
```

3. Run docker image:
```bash
docker run --rm -p 3838:3838 anomaly_detection
```

4. Open your favorite browser and navigate to the `http://127.0.0.1:3838/anomaly_detection/`
