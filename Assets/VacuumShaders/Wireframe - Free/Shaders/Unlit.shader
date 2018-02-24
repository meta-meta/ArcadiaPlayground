// https://www.facebook.com/VacuumShaders

Shader "VacuumShaders/The Amazing Wireframe/Preview"
{ 
    Properties 
    {
		_Color("Main Color (RGB)", color) = (1, 1, 1, 1)
		_MainTex("Base (RGB)", 2D) = "white"{}
		
		_V_WIRE_Color("Wire Color (RGB) Trans (A)", color) = (0, 0, 0, 1)	
		_V_WIRE_Size("Wire Size", Range(0, 0.5)) = 0.05
		
    }

    SubShader 
    {
		Tags { "RenderType"="Opaque" }

		Pass
	    {			  
		 
            CGPROGRAM 
		    #pragma vertex vert
	    	#pragma fragment frag
			#pragma target 3.0
			#pragma multi_compile_instancing
			#include "UnityCG.cginc"

			fixed4 _V_WIRE_Color;
			half _V_WIRE_Size;	

			fixed4 _Color;
			sampler2D _MainTex;
			half4 _MainTex_ST;


			struct vInput
			{
				float4 vertex : POSITION;
				half4 texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct vOutput
			{
				float4 pos :SV_POSITION;
				float2 uv : TEXCOORD0;
				fixed3 mass : TEXCOORD1;
				UNITY_VERTEX_OUTPUT_STEREO
			};

			vOutput vert(vInput v)
			{
				vOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				o.pos = UnityObjectToClipPos(v.vertex);
				o.uv = v.texcoord.xy * _MainTex_ST.xy + _MainTex_ST.zw;

				o.mass = fixed3(floor(v.texcoord.z), frac(v.texcoord.z) * 10, v.texcoord.w);

				return o;
			}

			fixed4 frag(vOutput i) : SV_Target 
			{	
				fixed4 retColor = tex2D(_MainTex, i.uv.xy) * _Color;
				 
				half3 width = abs(ddx(i.mass.xyz)) + abs(ddy(i.mass.xyz));
				half3 eF = smoothstep(half3(0, 0, 0), width * _V_WIRE_Size * 20, i.mass.xyz);		
	  			half value = min(min(eF.x, eF.y), eF.z);	
				
				return lerp(lerp(retColor, _V_WIRE_Color, _V_WIRE_Color.a), retColor, value);
			}


			ENDCG 

    	} //Pass
			
        
    } //SubShader


} //Shader
