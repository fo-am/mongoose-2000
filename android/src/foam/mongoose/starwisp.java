// Starwisp Copyright (C) 2013 Dave Griffiths
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package foam.mongoose;

import java.util.ArrayList;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.content.Context;
import android.graphics.Color;

import java.io.File;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import android.widget.TextView;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.SeekBar;
import android.widget.Spinner;
import android.widget.ArrayAdapter;
import android.widget.AdapterView;
import android.widget.EditText;
import android.widget.Toast;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.view.WindowManager;
import android.view.View;
import android.view.Gravity;
import android.view.KeyEvent;
import android.text.TextWatcher;
import android.text.Editable;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONArray;

import java.util.Calendar;

public class starwisp extends StarwispActivity
{
    static {
        // register all activities here
        ActivityManager.Register("splash",starwisp.class);
        ActivityManager.Register("main",MainActivity.class);
        ActivityManager.Register("experiments",ExperimentsActivity.class);
        ActivityManager.Register("pack-select",PackSelectActivity.class);
        ActivityManager.Register("individual-select",IndividualSelectActivity.class);
        ActivityManager.Register("pup-focal",PupFocalActivity.class);
        ActivityManager.Register("pup-focal-event",PupFocalEventActivity.class);
        ActivityManager.Register("event-self",EventSelfActivity.class);
        ActivityManager.Register("event-fed",EventFedActivity.class);
        ActivityManager.Register("event-aggression",EventAggressionActivity.class);

        ActivityManager.Register("manage-packs",ManagePacksActivity.class);
        ActivityManager.Register("new-pack",NewPackActivity.class);
        ActivityManager.Register("manage-individual",ManageIndividualActivity.class);
        ActivityManager.Register("new-individual",NewIndividualActivity.class);
        ActivityManager.Register("update-individual",UpdateIndividualActivity.class);

        ActivityManager.Register("tag-location",TagLocationActivity.class);
        ActivityManager.Register("sync",SyncActivity.class);
    };

    NetworkManager nm;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        setContentView(R.layout.main);

        String dirname = "mongoose/";
        m_AppDir = "/sdcard/"+dirname;
        File appdir = new File(m_AppDir);
        appdir.mkdirs();

        nm = new NetworkManager("mongoose-web",this);

        // build static things
        m_Scheme = new Scheme(this);
        m_Builder = new StarwispBuilder(m_Scheme);
        m_Name = "splash";

        // tell scheme the date
        final Calendar c = Calendar.getInstance();
        int day = c.get(Calendar.DAY_OF_MONTH);
        int month = c.get(Calendar.MONTH)+1;
        int year = c.get(Calendar.YEAR);

        // pass in a bunch of useful stuff
        m_Scheme.eval("(define dirname \"/sdcard/"+dirname+"\")(define date-day "+day+") (define date-month "+month+") (define date-year "+year+")");

        Log.i("starwisp","started, now running starwisp.scm...");
        m_Scheme.eval(m_Scheme.readRawTextFile(this, "starwisp.scm"));

        super.onCreate(savedInstanceState);
    }
}
