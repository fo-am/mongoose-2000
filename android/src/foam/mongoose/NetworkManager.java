package foam.mongoose;

import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.wifi.ScanResult;
import android.net.wifi.WifiManager;
import android.util.Log;
import android.widget.Toast;

import android.net.wifi.WifiManager;
import android.content.BroadcastReceiver;
import android.content.IntentFilter;

import android.net.wifi.WifiConfiguration;

import java.io.File;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.InputStream;

import java.net.URL;
import java.net.HttpURLConnection;
import android.os.SystemClock;

public class NetworkManager {

    public enum State {
        SCANNING, CONNECTED // what else?
    }

    WifiManager wifi;
	BroadcastReceiver receiver;
    State state;
    String SSID;

    NetworkManager(String ssid, Context c) {
		wifi = (WifiManager) c.getSystemService(Context.WIFI_SERVICE);
        state = State.SCANNING;
        SSID = ssid;
        wifi.startScan();
		receiver = new WiFiScanReceiver(SSID, this);
		c.registerReceiver(receiver, new IntentFilter(
                               WifiManager.SCAN_RESULTS_AVAILABLE_ACTION));
    }

    void Connect() {
        Log.i("starwisp", "Attemping connect to "+SSID);

        List<WifiConfiguration> list = wifi.getConfiguredNetworks();

        Boolean found = false;

        for( WifiConfiguration i : list ) {
            if(i.SSID != null && i.SSID.equals("\"" + SSID + "\"")) {
                found = true;
                Log.i("starwisp", "Connecting");
                state=State.CONNECTED;
                wifi.disconnect();
                wifi.enableNetwork(i.networkId, true);
                wifi.reconnect();
                Log.i("starwisp", "Connected");
                break;
            }
        }

        if (!found) {
            Log.i("starwisp", "adding wifi config");
            WifiConfiguration conf = new WifiConfiguration();
            conf.SSID = "\"" + SSID + "\"";

//conf.wepKeys[0] = "\"" + networkPass + "\"";
//conf.wepTxKeyIndex = 0;
//conf.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE);
//conf.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.WEP40);

            conf.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE);
            wifi.addNetwork(conf);
        }
        else
        {
            StartRequestThread();
        }

    }

    public void StartRequestThread() {
        Runnable runnable = new Runnable() {
	        public void run() {
                Request();
	        }
        };
        Thread mythread = new Thread(runnable);
        mythread.start();
    }

    private void Request() {
        try {
            SystemClock.sleep(7000);

            Log.i("starwisp", "Pinging URL");
            String u="http://192.168.2.1:8888/mongoose?function_name=ping";
            Log.i("starwisp",u);
            URL url = new URL(u);
            HttpURLConnection con = (HttpURLConnection) url
                .openConnection();

            con.setReadTimeout(10000 /* milliseconds */);
            con.setConnectTimeout(15000 /* milliseconds */);
            con.setRequestMethod("GET");
            con.setDoInput(true);
            // Starts the query
            con.connect();

            Log.i("starwisp", "Connection open");
            readStream(con.getInputStream());
            Log.i("starwisp", "read stream, ending");
        } catch (Exception e) {
            Log.i("starwisp",e.toString());
            e.printStackTrace();
        }
    }

    private void readStream(InputStream in) {
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(in));
            String line = "";
            while ((line = reader.readLine()) != null) {
                Log.i("starwisp",line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private class WiFiScanReceiver extends BroadcastReceiver {
        private static final String TAG = "WiFiScanReceiver";

        public WiFiScanReceiver(String ssid, NetworkManager netm) {
            super();
            SSID=ssid;
            nm = netm;
        }

        public String SSID;
        public NetworkManager nm;

        @Override
        public void onReceive(Context c, Intent intent) {
            List<ScanResult> results = nm.wifi.getScanResults();
            ScanResult bestSignal = null;

            if (nm.state==State.SCANNING) {
                Log.i("starwisp", "Scanning "+nm.state);


                for (ScanResult result : results) {
                    if (result.SSID.equals(SSID)) {
                        nm.Connect();
                        return;
                    }
                }

                if (nm.state==State.SCANNING) {
                    Log.i("starwisp", "REScanning "+nm.state);
                    nm.wifi.startScan();
                }
            }
        }
    }
}
