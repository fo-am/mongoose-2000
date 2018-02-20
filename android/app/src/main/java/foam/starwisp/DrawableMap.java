// Starwisp Copyright (C) 2016 Dave Griffiths
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

package foam.starwisp;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Rect;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.text.Html;
import android.view.Gravity;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentTransaction;

import android.content.Intent;

import com.google.android.gms.maps.CameraUpdate;
import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.GoogleMap;
import com.google.android.gms.maps.OnMapReadyCallback;
import com.google.android.gms.maps.Projection;
import com.google.android.gms.maps.SupportMapFragment;
import com.google.android.gms.location.places.AutocompleteFilter;
import com.google.android.gms.location.places.ui.PlaceAutocomplete.IntentBuilder;
import com.google.android.gms.location.places.ui.PlaceAutocomplete;
import com.google.android.gms.common.GooglePlayServicesRepairableException;
import com.google.android.gms.common.GooglePlayServicesNotAvailableException;
import com.google.android.gms.maps.model.LatLngBounds;

import com.google.android.gms.location.places.Place;
import com.google.android.gms.common.api.Status;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.CameraPosition;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PolygonOptions;
import com.google.android.gms.maps.model.CircleOptions;
import com.google.android.gms.maps.model.PolylineOptions;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.Vector;

public class DrawableMap {
    FrameLayout fram_map;
    Button scribble_button;
    Button trash_button;
    Button place_button;
    Boolean draw_mode;
    Boolean button_mode;
    GoogleMap map;
    String map_mode;
    String selected_polygon;
    String selected_polygon_name;
    int ID;

    boolean map_ready;
    double centre_lat;
    double centre_lon;
    float centre_zoom;

    static double last_centre_lat;
    static double last_centre_lon;
    static float last_centre_zoom;

    static {
        last_centre_lat=49.198935;
        last_centre_lon=2.988281;
        last_centre_zoom=4;
    }
    
    boolean draw_indicator;
    double indicator_lat;
    double indicator_lon;

    StarwispActivity m_Context;
    StarwispBuilder m_Builder;
    
    TextView m_instructions;

    Vector<LatLng> current_polygon;

    class Polygon {
        String m_UniqueID;
        String m_Name;
        Vector<String> m_Info;
        Vector<LatLng> m_Verts;
    }

    ViewGroup m_parent;

    Vector<Polygon> polygons;
    
    final int PLACE_AUTOCOMPLETE_REQUEST_CODE = 1092;

    public void init(int id, ViewGroup parent, StarwispActivity c, StarwispBuilder b, String mode) {
	m_parent=parent;
        map_ready = false;
        draw_mode = false;
        button_mode = false;
        m_Context=c;
        m_Builder=b;
        map_mode = mode;
        ID = id;
        current_polygon = new Vector<LatLng>();
        polygons = new Vector<Polygon>();
        centre_lat=49.198935;
        centre_lon=2.988281;
        centre_zoom=4;
        centre_lat=last_centre_lat;
	centre_lon=last_centre_lon;
        centre_zoom=last_centre_zoom;
	draw_indicator=false;
	indicator_lat=0;
	indicator_lon=0;

        FrameLayout outer_map = new FrameLayout(c);
        outer_map.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
							       FrameLayout.LayoutParams.FILL_PARENT));

        FrameLayout map_container = new FrameLayout(c);
        map_container.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
								   FrameLayout.LayoutParams.FILL_PARENT));

        map_container.setId(ID);
	SupportMapFragment mapfrag = SupportMapFragment.newInstance();
        FragmentTransaction fragmentTransaction = c.getSupportFragmentManager().beginTransaction();
        fragmentTransaction.add(ID,mapfrag);
        fragmentTransaction.commit();
        outer_map.addView(map_container);
	
        fram_map = new FrameLayout(c);
        fram_map.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.FILL_PARENT,
							      FrameLayout.LayoutParams.FILL_PARENT));
        outer_map.addView(fram_map);

	LinearLayout map_cont = new LinearLayout(c);
	map_cont.setOrientation(LinearLayout.VERTICAL);
	LinearLayout.LayoutParams lp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.FILL_PARENT,
								     LinearLayout.LayoutParams.FILL_PARENT);
	lp.gravity=Gravity.CENTER;
	map_cont.setLayoutParams(lp);
	fram_map.addView(map_cont);

	LinearLayout button_cont = new LinearLayout(c);
	button_cont.setOrientation(LinearLayout.HORIZONTAL);
	lp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT,
					   LinearLayout.LayoutParams.WRAP_CONTENT);
	lp.gravity=Gravity.CENTER;
	button_cont.setLayoutParams(lp);
	map_cont.addView(button_cont);


	StarwispActivity sw = (StarwispActivity) c;
	StarwispActivity.ResultHandler rh;
	sw.m_ResultHandler = sw.new ResultHandler() {
		@Override
		public void Result(int requestCode, int resultCode, Intent data) 
		{
		    if (requestCode == PLACE_AUTOCOMPLETE_REQUEST_CODE) {
			if (resultCode == PlaceAutocomplete.RESULT_ERROR) {
			    Status status = PlaceAutocomplete.getStatus(m_Context, data);
			    // TODO: Handle the error.
			    Log.i("starwisp", status.getStatusMessage());
			} else {
			    Place place = PlaceAutocomplete.getPlace(m_Context, data);
			    if (place!=null) {
				LatLng ll = place.getLatLng();
				LatLngBounds llb = place.getViewport();
				double radius = distanceTo(llb.southwest,llb.northeast)/2;
				double scale = radius / 500;
				int zoom = ((int) (16 - Math.log(scale) / Math.log(2)));
				Centre(ll.latitude, ll.longitude, zoom);
			    }
			} 		    
		    }
		}
	    };
	
        if (map_mode.equals("edit")) {	    
	    place_button = new Button(c);
	    LinearLayout.LayoutParams plp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT,
									  LinearLayout.LayoutParams.WRAP_CONTENT);
	    plp.gravity=Gravity.CENTER;
	    place_button.setLayoutParams(plp);
	    place_button.setTextSize(20);
	    place_button.setTypeface(((StarwispActivity) c).m_Typeface);
	    place_button.setText("Place search");
	    button_cont.addView(place_button);

	    place_button.setOnClickListener(new View.OnClickListener() {
		    @Override
		    public void onClick(View v) {
			try {
			    Intent intent =
				new PlaceAutocomplete.IntentBuilder(PlaceAutocomplete.MODE_FULLSCREEN)
				.build(m_Context);
			    m_Context.startActivityForResult(intent, PLACE_AUTOCOMPLETE_REQUEST_CODE);
			    //PLACE_AUTOCOMPLETE_REQUEST_CODE is integer for request code
			} catch (GooglePlayServicesRepairableException | GooglePlayServicesNotAvailableException e) {
			    // TODO: Handle the error.
			}
		    
		    }
		});


            scribble_button = new Button(c);
	    lp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT,
					       LinearLayout.LayoutParams.WRAP_CONTENT);
	    lp.gravity=Gravity.CENTER;
            scribble_button.setLayoutParams(lp);

            scribble_button.setTextSize(20);
            scribble_button.setTypeface(((StarwispActivity) c).m_Typeface);
            scribble_button.setText("Draw boundary");
            button_cont.addView(scribble_button);

            trash_button = new Button(c);
	    lp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT,
					       LinearLayout.LayoutParams.WRAP_CONTENT);
	    lp.gravity=Gravity.CENTER;
            trash_button.setLayoutParams(lp);
            trash_button.setTextSize(20);
            trash_button.setTypeface(((StarwispActivity) c).m_Typeface);
            trash_button.setText("Delete boundary");
            button_cont.addView(trash_button);
	    trash_button.setVisibility(View.GONE);

            trash_button.setOnClickListener(new View.OnClickListener() {
		    @Override
		    public void onClick(View v) {
			if (draw_mode) {
			    draw_indicator=false;
			    current_polygon.clear();
			} else {
			    RemoveSelected();
			}
			DrawMap();
		    }
		});
	    
	    
	    m_instructions = new TextView(c);
	    lp = new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT,
					       LinearLayout.LayoutParams.WRAP_CONTENT);
	    lp.gravity=Gravity.CENTER;
	    lp.setMargins(10,10,10,10);
	    m_instructions.setGravity(Gravity.CENTER_VERTICAL | Gravity.CENTER_HORIZONTAL);
	    m_instructions.setLayoutParams(lp);
	    m_instructions.setTextSize(20);
	    m_instructions.setTypeface(m_Context.m_Typeface);
	    m_instructions.setTextColor(Color.WHITE);
	    // arg i18n
            map_cont.addView(m_instructions);
	    
        } else {
            //button_mode=true;
        }


        parent.addView(outer_map);

	mapfrag.getMapAsync(new OnMapReadyCallback() {
            @Override
            public void onMapReady(GoogleMap googleMap) {
                map = googleMap;
                map.setMapType(GoogleMap.MAP_TYPE_SATELLITE);
        		map.setMyLocationEnabled(true);
	        	map.getUiSettings().setZoomControlsEnabled(true);
                SetupStuff();
                DrawMap();
                //CameraUpdate center_map=CameraUpdateFactory.newLatLng(new LatLng(centre_lat,centre_lon));
                //CameraUpdate zoom_map=CameraUpdateFactory.zoomTo(centre_zoom);
                //map.moveCamera(center_map);
                //map.animateCamera(zoom_map);
		
                CameraPosition cameraPosition = new CameraPosition.Builder()
                    .target(new LatLng(centre_lat, centre_lon)).zoom(centre_zoom).build();
		map.moveCamera(CameraUpdateFactory
			       .newCameraPosition(cameraPosition)); 
		
		map.setOnCameraMoveListener(new GoogleMap.OnCameraMoveListener() {
			@Override
			public void onCameraMove() {
			    LatLng pos = map.getCameraPosition().target;
			    last_centre_zoom = map.getCameraPosition().zoom;
			    last_centre_lat = pos.latitude;
			    last_centre_lon = pos.longitude;
			}
		    });
                
		map_ready=true;
	    }});

    }

    public void Clear() {
        current_polygon.clear();
        polygons.clear();
    }

    public void Centre(double lat, double lng, int z) {
        centre_lat = lat;
        centre_lon = lng;
        centre_zoom = z;
        last_centre_lat=centre_lat;
        last_centre_lon=centre_lon;
        last_centre_zoom=centre_zoom;

        if (map_ready) {
            CameraUpdate center_map=CameraUpdateFactory.newLatLng(new LatLng(centre_lat,centre_lon));
            CameraUpdate zoom_map=CameraUpdateFactory.zoomTo(centre_zoom);
            map.moveCamera(center_map);
            map.moveCamera(zoom_map);
        }
    }

    public void RemoveSelected() {
	Vector<Polygon> new_polygons = new Vector<Polygon>();
        for (Polygon poly : polygons) {
	    if (!poly.m_UniqueID.equals(selected_polygon)) {
		new_polygons.add(poly);
	    }
	}
	polygons = new_polygons;
    }

    public void UpdateName(String name) {
        for (Polygon poly : polygons) {
	    if (poly.m_UniqueID.equals(selected_polygon)) {
		poly.m_Name=name;
	    }
	}
	if (map_ready) {
	    DrawMap();
	}
    }
    
    public void UpdateFromJSON(JSONArray map) {
        Clear();
        // json format
        // [ current_polygon(id) [ polygon, polygon, ... ]]
        // polygon:
        // [ name uid infotext [ latlng, latlng, ...]]
        // latlng:

        // (map may not exist yet when called from update)
        try {
            selected_polygon = map.getString(0);
            JSONArray polygon_list = map.getJSONArray(1);
            for (int i=0; i<polygon_list.length(); i++) {

		Log.e("starwisp", "poly "+i);

                JSONArray poly = polygon_list.getJSONArray(i);
                Polygon new_poly = new Polygon();
                new_poly.m_Name = poly.getString(0);
                new_poly.m_UniqueID = poly.getString(1);
		JSONArray info_list= poly.getJSONArray(2);
		new_poly.m_Info = new Vector<String>();
		for (int j=0; j<info_list.length(); j++) {
		    new_poly.m_Info.add(info_list.getString(j));
		}

                JSONArray verts = poly.getJSONArray(3);

		// pick out the selected poly's name
		if (new_poly.m_UniqueID.equals(selected_polygon)) {
		    selected_polygon_name=new_poly.m_Name;
		    if (verts.length()==0) {
			trash_button.setVisibility(View.GONE);
			scribble_button.setText("Create boundary");
			m_instructions.setText("Zoom in to your field and press 'Create boundary' when you are ready.");
		    } else {
			trash_button.setVisibility(View.GONE);
			scribble_button.setText("Redraw boundary");
			m_instructions.setText("");
		    }
		}

                new_poly.m_Verts = new Vector<LatLng>();
                for (int v=0; v<verts.length(); v++) {
		    Log.e("starwisp", "vert "+v);
                    JSONArray latlng = verts.getJSONArray(v);
                    new_poly.m_Verts.add(new LatLng(latlng.getDouble(0),latlng.getDouble(1)));
                }
                if (new_poly.m_Verts.size()>0) {
                    polygons.add(new_poly);
                }
            }
        } catch (JSONException e) {
            Log.e("starwisp", "Error parsing data in drawable map " + e.toString());
        }

	if (map_ready) {
	    Log.e("starwisp", "Redrawing map after JSON update");
 	    DrawMap();
	}
    }



    public void SendPolygon(Vector<LatLng> polygon) {
        String str="(";
        for (LatLng latlng : polygon) {
            str+="("+Double.toString(latlng.latitude)+" "+Double.toString(latlng.longitude)+") ";
        }
        str+=")";
        m_Builder.CallbackArgs(m_Context,m_Context.m_Name,ID,str);
    }

    public void SetupStuff() {

        if (map_mode.equals("edit")) {
            scribble_button.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    draw_mode = !draw_mode;
                    if (!draw_mode) {
			if (current_polygon.size()>2) {
			    Polygon poly = new Polygon();
			    poly.m_Verts = current_polygon;
			    poly.m_Name = selected_polygon_name;
			    poly.m_UniqueID = selected_polygon;
			    polygons.add(poly);
			    SendPolygon(poly.m_Verts);
			}
                        current_polygon = new Vector<LatLng>();
                        scribble_button.setText("Redraw boundary");
			trash_button.setVisibility(View.GONE);
			place_button.setVisibility(View.VISIBLE);
			m_instructions.setText("");
			draw_indicator=false;
			DrawMap();
                    } else {
			RemoveSelected();
			DrawMap();
                        scribble_button.setText("Save boundary");
			place_button.setVisibility(View.GONE);
			trash_button.setVisibility(View.VISIBLE);
			m_instructions.setText("Describe the boundary of your field by creating points in a clockwise fashion. Press 'delete boundary' at any time to start again.");
                    }
                }
            });
        }

	fram_map.setOnTouchListener(new View.OnTouchListener() {
            @Override
            public boolean onTouch(View v, MotionEvent event) {
                if (!draw_mode && !button_mode) return false;

                float x = event.getX();
                float y = event.getY();

                int x_co = Math.round(x);
                int y_co = Math.round(y);

                Projection projection = map.getProjection();
                Point x_y_points = new Point(x_co, y_co);

                LatLng latLng = map.getProjection().fromScreenLocation(x_y_points);
                double latitude = latLng.latitude;
                double longitude = latLng.longitude;

                int eventaction = event.getAction();
                switch (eventaction) {
                    case MotionEvent.ACTION_DOWN:
                        // finger touches the screen
                        if (map_mode.equals("edit")) {
			    draw_indicator=true;
			    indicator_lat = latitude;
			    indicator_lon = longitude;
                            current_polygon.add(new LatLng(latitude, longitude));
			    // hide the instructions so they don't get in the way
			    m_instructions.setText("");
                        } else {
                            String clicked_in = CheckPolygons(latitude, longitude);
                            if (!clicked_in.equals("")) {
                                m_Builder.CallbackArgs(m_Context,m_Context.m_Name,ID,"\""+clicked_in+"\"");
                            }
                        }
                        break;

                    case MotionEvent.ACTION_MOVE:
                        // finger moves on the screen
                        break;

                    case MotionEvent.ACTION_UP:
                        // finger leaves the screen
                        DrawMap();
                        break;
                }

                if (button_mode) return false;

                return true;

            }
	    });
    }

    double distanceTo(LatLng lla, LatLng llb) {
	double earthRadius = 3958.75;
	double latDiff = Math.toRadians(llb.latitude-lla.latitude);
	double lngDiff = Math.toRadians(llb.longitude-lla.longitude);
	double a = Math.sin(latDiff /2) * Math.sin(latDiff /2) +
	    Math.cos(Math.toRadians(lla.latitude)) * Math.cos(Math.toRadians(llb.latitude)) *
	    Math.sin(lngDiff /2) * Math.sin(lngDiff /2);
	double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
	double distance = earthRadius * c;
	int meterConversion = 1609;
	return new Float(distance * meterConversion).floatValue();
    }

    LatLng GetCentre(Polygon poly) {
        Double centrex = 0.0;
        Double centrey = 0.0;
        for (LatLng latlng : poly.m_Verts) {
            centrex+=latlng.latitude;
            centrey+=latlng.longitude;
        }
        centrex/=poly.m_Verts.size();
        centrey/=poly.m_Verts.size();
        return new LatLng(centrex,centrey);
     }

    Boolean IsInPolygon(Polygon poly, double x, double y) {
        int intersectionCount = 0;
        double x0 = poly.m_Verts.lastElement().latitude - x;
        double y0 = poly.m_Verts.lastElement().longitude - y;
        for (LatLng vert : poly.m_Verts) {
            double x1 = vert.latitude - x;
            double y1 = vert.longitude - y;
            if (y0 > 0 && y1 <= 0 && x1 * y0 > y1 * x0) {
                intersectionCount++;
            }
            if (y1 > 0 && y0 <= 0 && x0 * y1 > y0 * x1) {
                intersectionCount++;
            }
            x0 = x1;
            y0 = y1;
        }
        return (intersectionCount % 2) == 1;
    }

    String CheckPolygons(double x, double y) {
        for (Polygon poly : polygons) {
            if (IsInPolygon(poly,x,y)) {
                return poly.m_UniqueID;
            }
        }
        return "";
    }

    public void DrawMap() {
        map.clear();

        for (Polygon poly : polygons) {
            PolygonOptions rectOptions = new PolygonOptions();
            rectOptions.addAll(poly.m_Verts);
            if (selected_polygon.equals(poly.m_UniqueID)) {
                rectOptions.strokeColor(0x77ffff55);
            } else {
                rectOptions.strokeColor(0x77aaFFaa);
            }
            rectOptions.strokeWidth(3);
            rectOptions.fillColor(0x30aaFFaa);
            map.addPolygon(rectOptions);

	    // only show text for one field in edit mode
	    if (!map_mode.equals("edit")) {
		final float scale = m_Context.getResources().getDisplayMetrics().density;
		AddText(GetCentre(poly), poly.m_Name, (int)(40.0*scale), 20, Color.WHITE);
		for (int j=0; j<poly.m_Info.size(); j++) {
		    AddText(GetCentre(poly), poly.m_Info.get(j), j*(int)(20.0*scale), 14, 0xffccFFcc);
		}
	    } else {
		if (selected_polygon.equals(poly.m_UniqueID)) {
		    AddText(GetCentre(poly), poly.m_Name, 0, 20, Color.WHITE);
		}
	    }

        }

        if (current_polygon.size()!=0) {
            PolygonOptions rectOptions = new PolygonOptions();
            rectOptions.addAll(current_polygon);
            rectOptions.strokeColor(0x77ffff55);
            rectOptions.strokeWidth(3);
            rectOptions.fillColor(0x30aaFFaa);
            map.addPolygon(rectOptions);
        }

	if (draw_indicator) {
	    CircleOptions iOptions = new CircleOptions();
            iOptions.center(new LatLng(indicator_lat,indicator_lon));
            iOptions.radius(50);
            iOptions.strokeColor(0xffffffff);
            iOptions.strokeWidth(1);
            iOptions.fillColor(0x00000000);
            map.addCircle(iOptions);
	    {
		PolylineOptions pOptions = new PolylineOptions();
		pOptions.add(new LatLng(indicator_lat+0.0007,indicator_lon));
		pOptions.add(new LatLng(indicator_lat-0.0007,indicator_lon));
		pOptions.color(0xffffffff);
		pOptions.width(1);
		map.addPolyline(pOptions);
	    }
	    {
		PolylineOptions pOptions = new PolylineOptions();
		pOptions.add(new LatLng(indicator_lat,indicator_lon+0.001));
		pOptions.add(new LatLng(indicator_lat,indicator_lon-0.001));
		pOptions.color(0xffffffff);
		pOptions.width(1);
		map.addPolyline(pOptions);
	    }
	}
    }

    public Marker AddText(final LatLng location, final String text, final int padding, final int fontSize, int colour) {
        Marker marker = null;
	if (text.equals("")) return marker;

        final TextView textView = new TextView(m_Context);
        textView.setText(text);
        textView.setTextSize(fontSize);
        textView.setTypeface(m_Context.m_Typeface);

        final Paint paintText = textView.getPaint();

        final Rect boundsText = new Rect();
        paintText.getTextBounds(text, 0, textView.length(), boundsText);
        paintText.setTextAlign(Paint.Align.CENTER);

        final Bitmap.Config conf = Bitmap.Config.ARGB_8888;
        final Bitmap bmpText = Bitmap.createBitmap(boundsText.width() + 2
              * padding, boundsText.height() + 2 * padding, conf);

        final Canvas canvasText = new Canvas(bmpText);
        paintText.setColor(Color.BLACK);
	
        canvasText.drawText(text, (canvasText.getWidth() / 2)+3,
			    (canvasText.getHeight() - padding - boundsText.bottom)+3, paintText);
	
        paintText.setColor(colour);
	
        canvasText.drawText(text, canvasText.getWidth() / 2,
			    canvasText.getHeight() - padding - boundsText.bottom, paintText);
	
        final MarkerOptions markerOptions = new MarkerOptions()
                .position(location)
                .icon(BitmapDescriptorFactory.fromBitmap(bmpText))
                .anchor(0.5f, 1);

        marker = map.addMarker(markerOptions);

        return marker;
    }
}


