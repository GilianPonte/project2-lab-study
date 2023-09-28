Qualtrics.SurveyEngine.addOnload(function()
{
	/*Place your JavaScript here to run when the page loads*/

});

Qualtrics.SurveyEngine.addOnReady(function()
{
	/*Place your JavaScript here to run when the page is fully displayed*/
	/**
 * wheel.js
 * 
 * For demo see wheel.html
 * 
 * @copyright 2013 ETH Zurich, www.socio.ethz.ch, c/o Marc Hoeglinger <hoeglinger@soz.gess.ethz.ch>
 * @license http://www.gnu.org/licenses/gpl-2.0.txt
 * @author Philip Tschiemer <tschiemer@filou.se>
 * @link https://github.com/tschiemer/qualtrics-gambling
 * @version 2013-10-01
 */


    function Wheel(wheel_id, btn_rotate_id, options)
    {
        if (wheel_id == undefined || btn_rotate_id == undefined || options == undefined)
        {
            alert('Parameters not set. Please make sure to set <wheel_id>, <btn_rotate_id> and <options>.');
            return;
        }
        
        // Dice initialization
        
        // Required for correct referencing in anonymous functions
        var self = this;

        // Is Dice currently being thrown?
        this.is_busy = false;

        // Number of throws that can be made before controls are deactivated
        this.possible_throws = options.possible_throws == undefined ? 0 : options.possible_throws;
        
        // Number of throws currently done.
        this.done_throws     = 0;

        // Reference to dice DOM container
        this.wheel       = document.getElementById(wheel_id);         
        
        // Reference to throw button DOM
        this.btn_rotate  = document.getElementById(btn_rotate_id);
        
        // Set of all possible values following the structure
        // {key: "my_key", probability: 1} where the key field will be passed to any callback
        this.value_set = options.value_set;
        
        // Set of all predefined result keys, either an array of key values or
        // a string with comma-separated keys, ie
        // ['key1','key2',..,'keyN']   OR   'key1,key2,..,keyN'
        this.result_set = [];
        if (typeof options.result_set != 'undefined')
        {   
            this.first_throw_nr = 0;
            if (typeof options.first_throw_nr == 'string')
            {
                this.first_throw_nr = parseInt(options.first_throw_nr);
            }
            else if (typeof options.first_throw_nr == 'number')
            {
                this.first_throw_nr = Math.floor(options.first_throw_nr);
            }
            
//            var throw_index_strategy = 'couple';
//            if (typeof options.throw_index_strategy == 'string')
//            {
//                throw_index_strategy = options.throw_index_strategy;
//            }
//            if (throw_index_strategy == 'couple')
//            {
//                this.done_throws = this.first_throw_nr;
//            }
            
            if (typeof options.result_set == 'string')
            {
               this.result_set = options.result_set.replace(/ /g,'').split(',');
            }
            else
            {
                this.result_set = options.result_set;
            }
            
            if (this.possible_throws == 0)
            {
                this.possible_throws = this.result_set.length - this.first_throw_nr;
//                if (this.possible_throws +  > this.result_set.length)
//                {
//                    this.possible_throws = this.result_set.length - this.first_throw_nr;
//                }
            }
            
            this.fallback_strategy = 'wrap-around';
            if (typeof options.fallback_strategy == 'string')
            {
                this.fallback_strategy = options.fallback_strategy;
            }
        }
        
        // Duration of roll/throw animation, resp duration until result is shown.
        // Can be number or function returning a number
        // Default 1
        // In Seconds.
        this.roll_duration = options.roll_duration == undefined ? 1 : options.roll_duration;
        
        // Callback function to receive any results
        // Signature: result_callback( throw_nr, result_key )
        //  throw_nr    : number of current throw starting with 1
        //  result_key  : key of throw result, as given through value_set 
        this.result_callback = null;
        if (options.result_callback !== undefined)
        {
            this.result_callback = options.result_callback;
        }
        
        if (this.possible_throws > 0 && options.finished_callback !== undefined)
        {
            this.finished_callback = options.finished_callback;
        }
        
        this.current_value_idx = 0;
        this.current_value = function()
        {
            return this.value_set[this.current_value_idx];
        }
        
        // default Animation options, can NOT be overriden
        this.animation = {
            mode                : (typeof options.animation_mode == 'string') ? options.animation_mode : 'sequence',
            tick_interval       : 10,
            max_degree          : 9,
            step_interpolator: function(x){
                if (x < 0) return 1;
                if (x > 1) return 0;
                if (x == 0) return 0.5; // slow start
                
                // interpolate between two different exponentials for a
                // longer high value and a less sudden drop
                return (1-x)*(1-Math.pow(x,4)) + (x)*(1-Math.pow(x,2));
            }
        };
        if (360 / this.value_set.length < this.animation.max_degree)
        {
            this.animation.max_degree = 360 / this.value_set.length;
        }
        
        // Store image options 
        if (options.images == undefined)
        {
            alert('Image settings missing.');
            return;
        }
        this.images = options.images;
        
        
        // Sanity checks: could DOM elements be found?
        if (this.wheel == null){
            alert('Dice ID not properly set, could not find an element with id '+dice_id);
            return;
        }
        if (this.btn_rotate == null){
            alert('Throw button ID not properly set, could not find an element.'+btn_throw_id);
            return;
        }


        //// Finalize Value Sets (sanity check and normalize probablities)
        var sum = 0, count=0;
        for(var v = 0; v < this.value_set.length; v++)
        {
            sum += this.value_set[v].probability;
            count++;
        }
        if (count == 0)
        {
            alert('No possible results entered.');
            return;
        }
        // if probablity sum = 0, assume uniform probablitiy distribution
        if (sum == 0)
        {
            var p = 1 / count;
            for (var v = 0; v < this.value_set.length; v++)
            {
                this.value_set[v].probability = p;
            }
        }
        else
        {
            for(var v = 0; v < this.value_set.length; v++)
            {
                this.value_set[v].probability /= sum;
            }
        }


        //// Setup HTML / Style
        
        this.wheel.innerHTML = '';
        this.wheel.style.height = this.images.img_size + 'px';
        this.wheel.style.overflow = 'hidden';
        
        
        // create colors image
        this.color_img = document.createElement('img');
        this.color_img.style.width      = this.images.img_size + 'px';
        this.color_img.style.height      = this.images.img_size + 'px';
        
        // create label image
        this.label_img = document.createElement('img');
        this.label_img.style.position = 'relative';
        this.label_img.style.top      = -this.images.img_size +'px';
        this.label_img.style.width    = this.images.img_size + 'px';
        this.label_img.style.height    = this.images.img_size + 'px';
        
//        if (this.animation.mode == 'sequence')
//        {
            this.color_img.src = this.images.base_url + this.current_value().color_img;
            this.label_img.src = this.images.base_url + this.current_value().label_img;
//        }
//        else if (this.animation.mode == 'crop')
//        {
//            var size = '0px, '+this.images.img_size + 'px,'+this.images.img_size+'px,0px';
//            
//            this.color_img.src = this.images.base_url + this.images.color_img;
//            this.color_img.style.clip = 'rect('+size+')';
//            this.label_img.src = this.images.base_url + this.images.label_img;
//            this.color_img.style.clip = 'rect('+size+')';
//        }

        this.wheel.appendChild(this.color_img);
        this.wheel.appendChild(document.createElement('br'));
        this.wheel.appendChild(this.label_img);
        this.wheel.appendChild(document.createElement('br'));
        
        
        if (typeof this.images.select_img == 'string')
        {
            var arrow_img = document.createElement('img');
            arrow_img.src            = this.images.base_url + this.images.select_img;
            arrow_img.style.position = 'relative';
            arrow_img.style.top      = -2*this.images.img_size +'px';
            arrow_img.style.width    = this.images.img_size + 'px';
            arrow_img.style.height    = this.images.img_size + 'px';

            this.wheel.appendChild(arrow_img);
            this.wheel.appendChild(document.createElement('br'));
        }
        
        // create final result description text
        this.desc_text = document.createElement('span');
        this.desc_text.style.display   = 'inline-block';
        this.desc_text.style.position = 'relative';
        var shift  = (typeof this.images.select_img == 'string' ? -3 : -2);
        this.desc_text.style.top      = (shift*this.images.img_size) +'px';
        this.desc_text.style.width      = this.images.img_size + 'px';
        this.desc_text.style.lineHeight = this.images.img_size + 'px';
        this.desc_text.style.textAlign  = 'center';
        this.desc_text.innerHTML        = '&nbsp;';
        this.wheel.appendChild(this.desc_text);
        

        // preload color and label images
        if (this.animation.mode == 'sequence')
        {
            for(var v = 0; v < this.value_set.length; v++)
            {
                var img;

                img = document.createElement('img');
                img.style.display = 'none';
                img.src = this.images.base_url + this.value_set[v].color_img;
                this.wheel.appendChild(img);

                img = document.createElement('img');
                img.style.display = 'none';
                img.src = this.images.base_url + this.value_set[v].label_img;
                this.wheel.appendChild(img);
            }
        }
//        else if (this.animation.mode == 'crop')
//        {
//            var img;
//            
//            img.document.createElement('imt');
//            img.style.display = 'none';
//            
//        }
        
        
        //// Object Methods
        
        
        this.deactivate_controls = function()
        {
            this.btn_rotate.disabled = true;
        }

        this.activate_controls = function()
        {
            this.btn_rotate.disabled = false;
        }


        // Throw dice
        this.rotate = function()
        {
            if (this.possible_throws > 0 && this.done_throws >= this.possible_throws)
            {
                return;
            }
            if (this.is_busy)
            {
                return;
            }
            this.is_busy = true;
            this.deactivate_controls();
            
            this.desc_text.innerHTML = '&nbsp;';

            // Determine result ..
            var result = null;
            
            // .. by relying on the given result
            if (this.result_set.length > 0 && (this.first_throw_nr + this.done_throws < this.result_set.length || this.fallback_strategy == 'wrap-around'))
            {
                var idx = (this.first_throw_nr + this.done_throws) % this.result_set.length;
                var result_key = this.result_set[idx];
                for(var i=0; i < this.value_set.length; i++)
                {
                    if (this.value_set[i].key == result_key)
                    {
                        result_idx = i;
                        break;
                    }
                }
            }
            // .. or by randomly drawing one.
            else
            {
                // Determine result (Monte Carlo style, 1 sample..)
                var p = Math.random();
                var sum = 0;
    //            var result = null;
                var result_idx = null;
                for(var v in this.value_set)
                {
                    sum += this.value_set[v].probability;
                    if (sum >= p)
                    {
    //                    result = this.value_set[v];
                        result_idx = v;
                        break;
                    }
                }
            }
            
//            alert('result = '+result_idx);
            
            var n_fields = 1 * this.value_set.length;
            if (result_idx < this.current_value_idx)
            {
                n_fields += this.current_value_idx  - result_idx;
            }
            else if (result_idx > this.current_value_idx)
            {
                n_fields += this.current_value_idx - result_idx;
            }
//            alert('n_fields = '+n_fields);
            

            // Start animation and tell it how many fields it must 
            this.start_animation(n_fields);            
            
        }
        this.btn_rotate.onclick = function(){self.rotate();return false;};


        // Animation internals
        
        this.animate_interval = 0;
        this.animate_field_countdown = 0;
        this.animate_degrees_to_next = 360 / this.value_set.length;

        // Start animation lasting <roll_duration> microseconds.
        this.start_animation = function(n_fields)
        {
            // If animation is alraedy running abort.
            if (this.animate_interval != 0)
            {
                return;
            }
            
            // Setup variables for actual animation tick function
            var self = this;
            this.animate_field_countdown = n_fields;

            // .. and start ticker.
            this.animate_interval = window.setInterval(function(){
                self.animation_tick(n_fields);
            },this.animation.tick_interval);
        }

        // Animation Step / Tick / Redraw function
        this.animation_tick = function(total_fields)
        {
            var deg_per_value = 360 / this.value_set.length;
            
            var rotate_step = this.animation.max_degree * this.animation.step_interpolator(1 - this.animate_field_countdown / total_fields);
            if (rotate_step <= 0 )
            {
                rotate_step = 1;
            }
            
            // Rotate images
            // Default rotation
            var deg = 0;

            // If image is already rotated, get current rotation degree.
            if (this.color_img.style.transform !== undefined)
            {
                deg = +this.color_img.style.transform.replace(/rotate\(|deg\)/g,'');

            }

            if (this.animate_degrees_to_next < rotate_step)
            {
                // update animation settings
                
                this.animate_field_countdown--;
                
                if (this.current_value_idx == 0)
                {
                    this.current_value_idx = this.value_set.length - 1;
                }
                else
                {
                    this.current_value_idx--;
                }
                
                // swap images
//                if (this.animation.mode == 'sequence')
                {
                    this.color_img.src = this.images.base_url + this.current_value().color_img;
                    this.label_img.src = this.images.base_url + this.current_value().label_img;
                }
//                else
//                {
//                    var size_color = '0px, '+((this.current_value_idx%2)*this.images.img_size) + 'px,'+((1 + this.current_value_idx%2)*this.images.img_size)+'px,0px';
//                        
//                    this.color_img.style.clip = 'rect('+size_color+')';
//                }
                
                
                // adjust rotation step to respect already rotated images
                rotate_step -= deg_per_value;
            }
            
            this.animate_degrees_to_next -= rotate_step;

            // Update rotation degree
            deg += rotate_step;

            // Set CSS rotation image
            
//            var iecos = Math.cos(deg * Math.PI / 180);
//            var iesin = Math.sin(deg * Math.PI / 180);
            
            this.color_img.style.transform = 'rotate('+deg+'deg)';
            this.color_img.style['-webkit-transform'] = 'rotate('+deg+'deg)';
            this.color_img.style['-ms-transform'] = 'rotate('+deg+'deg)';
            this.color_img.style['-moz-transform'] = 'rotate('+deg+'deg)';
            this.color_img.style['-o-transform'] = 'rotate('+deg+'deg)';
//            this.color_img.style['filter'] = 'progid:DXImageTransform.Microsoft.Matrix(M11='+iecos+',M21='+iesin+',M22='+iecos+',M12='+(-iesin)+', sizingMethod="auto expand")';
            
            this.label_img.style.transform = 'rotate('+deg+'deg)';
            this.label_img.style['-webkit-transform'] = 'rotate('+deg+'deg)';
            this.label_img.style['-ms-transform'] = 'rotate('+deg+'deg)';
            this.label_img.style['-moz-transform'] = 'rotate('+deg+'deg)';
            this.label_img.style['-o-transform'] = 'rotate('+deg+'deg)';
//            this.label_img.style['filter'] = 'progid:DXImageTransform.Microsoft.Matrix(M11='+iecos+',M21='+iesin+',M22='+iecos+',M12='+(-iesin)+', sizingMethod="auto expand")';

            if ( this.animate_field_countdown <= 0)
            {
                this.show_result(this.current_value());
            }
        }

        // Stop all animation
        this.stop_animation = function()
        {
            // If animation ticker is not active, abort
            if (this.animate_interval == 0)
            {
                return;
            }

            // Clear animatino ticker
            window.clearInterval(this.animate_interval);
            this.animate_interval = 0;
        }

        // Show throw result and enabled controls again.
        this.show_result = function(result)
        {
            this.stop_animation();

//            this.color_img.
//            this.dice_img.src = this.images.base_url + result.key + this.images.ext; 

            this.is_busy = false;
            this.done_throws++;
            
            if (typeof result.description == 'string')
            {
                this.desc_text.innerHTML = result.description;
            }
            
            // If a callback function has been set, call it.
            if (this.result_callback != null)
            {
                this.result_callback(this.done_throws, result.key);
            }

            if (this.possible_throws == 0 || this.done_throws < this.possible_throws)
            {
                this.activate_controls();
            }
            else if (typeof this.finished_callback != 'undefined')
            {
                this.finished_callback();
            }
        }
    }

    // Alternative initialization function to be used when not 
    function wheel_create_here(btn_rotate_label, options)
    {
        // Generate random HTML tag ids.
        var wheel_id         = 'wheel_' + Math.floor(1000*Math.random());
        var btn_rotate_id    = 'btn_rotate_' + Math.floor(1000*Math.random());

        // Add required HTML elements to document.
        document.write('<div id="'+wheel_id+'"></div>');
        document.write('<button id="'+btn_rotate_id+'">'+btn_rotate_label+'</button>');
        
        // Initialize dice by standard method. Use little safety delay.
        window.setTimeout(function(){new Wheel(wheel_id, btn_rotate_id, options);},10);
    }
    
    
    // If you load this file dynamically and want to run your custom code when it has finished
    // loading, you can defined <dive_js_load_callback> as your callback function
    if (typeof wheel_js_load_callback != 'undefined')
    {
        wheel_js_load_callback();
    }
        var options = {
            possible_throws: 1,
            value_set: [
                {key:'truthfully',probability: 1, description: 'Truthfully', color_img:'color-1.jpg',label_img:'label-1.png'},
                {key:'truthfully',probability: 1, description: 'Truthfully', color_img:'color-2.jpg',label_img:'label-2.png'},
                {key:'truthfully',probability: 1, description: 'Truthfully', color_img:'color-3.jpg',label_img:'label-3.png'},
                {key:'truthfully',probability: 1, description: 'Truthfully', color_img:'color-4.jpg',label_img:'label-4.png'},
				{key:'truthfully',probability: 1, description: 'Truthfully', color_img:'color-5.jpg',label_img:'label-5.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-6.jpg',label_img:'label-6.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-7.jpg',label_img:'label-7.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-8.jpg',label_img:'label-8.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-9.jpg',label_img:'label-9.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-10.jpg',label_img:'label-10.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-11.jpg',label_img:'label-11.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-12.jpg',label_img:'label-12.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-13.jpg',label_img:'label-13.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-14.jpg',label_img:'label-14.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-15.jpg',label_img:'label-15.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-16.jpg',label_img:'label-16.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-17.jpg',label_img:'label-17.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-18.jpg',label_img:'label-18.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-19.jpg',label_img:'label-19.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-20.jpg',label_img:'label-20.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-21.jpg',label_img:'label-21.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-22.jpg',label_img:'label-22.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-23.jpg',label_img:'label-23.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-24.jpg',label_img:'label-24.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-25.jpg',label_img:'label-25.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-26.jpg',label_img:'label-26.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-27.jpg',label_img:'label-27.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-28.jpg',label_img:'label-28.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-29.jpg',label_img:'label-29.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-30.jpg',label_img:'label-30.png'},               
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-31.jpg',label_img:'label-31.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-32.jpg',label_img:'label-32.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-33.jpg',label_img:'label-33.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-34.jpg',label_img:'label-34.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-35.jpg',label_img:'label-35.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-36.jpg',label_img:'label-36.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-37.jpg',label_img:'label-37.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-38.jpg',label_img:'label-38.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-39.jpg',label_img:'label-39.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-40.jpg',label_img:'label-40.png'}, 
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-41.jpg',label_img:'label-41.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-42.jpg',label_img:'label-42.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-43.jpg',label_img:'label-43.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-44.jpg',label_img:'label-44.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-45.jpg',label_img:'label-45.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-46.jpg',label_img:'label-46.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-47.jpg',label_img:'label-47.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-48.jpg',label_img:'label-48.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-49.jpg',label_img:'label-49.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-50.jpg',label_img:'label-50.png'}, 
        {key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-51.jpg',label_img:'label-51.png'},
        {key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-52.jpg',label_img:'label-52.png'},
        {key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-53.jpg',label_img:'label-53.png'},
        {key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-54.jpg',label_img:'label-54.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-55.jpg',label_img:'label-55.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-56.jpg',label_img:'label-56.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-57.jpg',label_img:'label-57.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-58.jpg',label_img:'label-58.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-59.jpg',label_img:'label-59.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-60.jpg',label_img:'label-60.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-61.jpg',label_img:'label-61.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-62.jpg',label_img:'label-62.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-63.jpg',label_img:'label-63.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-64.jpg',label_img:'label-64.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-65.jpg',label_img:'label-65.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-66.jpg',label_img:'label-66.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-67.jpg',label_img:'label-67.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-68.jpg',label_img:'label-68.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-69.jpg',label_img:'label-69.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-70.jpg',label_img:'label-70.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-71.jpg',label_img:'label-71.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-72.jpg',label_img:'label-72.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-73.jpg',label_img:'label-73.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-74.jpg',label_img:'label-74.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-75.jpg',label_img:'label-75.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-76.jpg',label_img:'label-76.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-77.jpg',label_img:'label-77.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-78.jpg',label_img:'label-78.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-79.jpg',label_img:'label-79.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-80.jpg',label_img:'label-80.png'},               
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-81.jpg',label_img:'label-81.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-82.jpg',label_img:'label-82.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-83.jpg',label_img:'label-83.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-84.jpg',label_img:'label-84.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-85.jpg',label_img:'label-85.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-86.jpg',label_img:'label-86.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-87.jpg',label_img:'label-87.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-88.jpg',label_img:'label-88.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-89.jpg',label_img:'label-89.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-90.jpg',label_img:'label-90.png'}, 
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-91.jpg',label_img:'label-91.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-92.jpg',label_img:'label-92.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-93.jpg',label_img:'label-93.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-94.jpg',label_img:'label-94.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-95.jpg',label_img:'label-95.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-96.jpg',label_img:'label-96.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-97.jpg',label_img:'label-97.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-98.jpg',label_img:'label-98.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-99.jpg',label_img:'label-99.png'},
				{key:'disguised',probability: 1, description: 'Proceed to the next page', color_img:'color-100.jpg',label_img:'label-100.png'}
            ],
			roll_duration: 10,
            images: {
                base_url: 'https://raw.githubusercontent.com/GilianPonte/project2-lab-study/main/spinners 1 - answers 2/0.1000835/',
                img_size: 300,
				select_img: 'arrow.png'
            },
			result_callback: function(throw_nr, result_key){
            Qualtrics.SurveyEngine.setEmbeddedData('result', result_key);
        }
        };

        new Wheel('wheel','btn_rotate',options);
});

Qualtrics.SurveyEngine.addOnUnload(function()
{
	/*Place your JavaScript here to run when the page is unloaded*/

});
