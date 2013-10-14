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
import java.util.HashMap;
import java.lang.InstantiationException;
import java.lang.IllegalAccessException;

import android.app.Activity;
import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.content.Context;
import android.view.ViewGroup;
import android.view.View;

public class ActivityManager {
    static private HashMap<String,Class> m_Activities;
    static private HashMap<String,Class> m_Fragments;

    static {
        m_Activities = new HashMap<String,Class>();
        m_Fragments = new HashMap<String,Class>();
    }

    static public void RegisterActivity(String name, Class actclass)
    {
        m_Activities.put(name,actclass);
    }

    static public void RegisterFragment(String name, Class fragclass)
    {
        m_Fragments.put(name,fragclass);
    }

    static public void StartActivity(Activity src, String name, int requestcode, String arg)
    {
        Class ActClass = m_Activities.get(name);
        if (ActClass == null)
        {
            Log.i("starwisp","activity "+name+" not found in registry");
        }
        else
        {
            Intent intent = new Intent(src,ActClass);
            intent.putExtra("arg", arg);
            src.startActivityForResult(intent, requestcode);
        }
    }

    static public void StartActivityGoto(Activity src, String name, String arg)
    {
        Class ActClass = m_Activities.get(name);
        if (ActClass == null)
        {
            Log.i("starwisp","activity "+name+" not found in registry");
        }
        else
        {
            Intent intent = new Intent(src,ActClass);
            intent.putExtra("arg", arg);
            src.startActivity(intent);
        }
    }

    static public Fragment GetFragment(String name)
    {
        Class FragClass = m_Fragments.get(name);
        if (FragClass == null)
        {
            Log.i("starwisp","fragment "+name+" not found in registry");
        }
        try {
            return (Fragment)FragClass.newInstance();
        } catch (InstantiationException e) {
            Log.i("starwisp","fragment "+name+" error InstantiationException");
            return null;
        } catch (IllegalAccessException e) {
            Log.i("starwisp","fragment "+name+" error IllegalAccessException");
            return null;
        }
    }


}
