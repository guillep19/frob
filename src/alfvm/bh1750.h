#ifndef BH1750_H
#define BH1750_H
 
#include "mbed.h"
#include "FrobDefinitions.h"
 
// light intensity sensor, BH1750
// 7bit address = 0b0100011(0x23) or 0b1011100(0x5c)
#define BH1750_G_CHIP_ADDR         (0x23 << 1)
#define BH1750_V_CHIP_ADDR         (0x5c << 1)
 
////////////// COMMAND ////////////////////////////////////
#define CMD_PWR_DWN                0x00
#define CMD_PWR_UP                 0x01
#define CMD_RESET                  0x07
#define CMD_C_H_RES_M              0x10
#define CMD_C_H_RES_M2             0x11
#define CMD_C_L_RES_M              0x13
#define CMD_1_H_RES_M              0x20
#define CMD_1_H_RES_M2             0x21
#define CMD_1_L_RES_M              0x23
#define CMD_M_TIME_H               0x40
#define CMD_M_TIME_L               0x60
 
////////////// SENSITIVITY ////////////////////////////////
#define SENS_1R00                  69
#define SENS_3R68                  254
#define SENS_0R45                  31
#define SENS_2R00                  138
#define SENS_0R50                  35
 
/** Interface for Luminosity sensor, BH1750
 * @code
 * #include "mbed.h"
 * #include "BH1750.h"
 *
 * // I2C Communication
 *  BH1750      lum(dp5,dp27);    // BH1750 SDA, SCL
 * // If you connected I2C line not only this device but also other devices,
 * //     you need to declare following method.
 *  I2C         i2c(dp5,dp27);    // SDA, SCL
 *  BH1750      lum(i2c);         // BH1750 SDA, SCL (Data available every 120mSec)
 *
 * int main() {;
 *   while(true){
 *      printf("Illuminance: %+7.2f [Lux]\r\n", lum.lux());
 *      wait(1.0);
 *   }
 * }
 * @endcode
 */
 
class BH1750
{
public:
    /** Configure data pin
      * @param data SDA and SCL pins
      */
    BH1750(PinName p_sda, PinName p_scl);
    BH1750(PinName p_sda, PinName p_scl, uint8_t addr);
 
    /** Configure data pin (with other devices on I2C line)
      * @param I2C previous definition
      */
    BH1750(I2C& p_i2c);
    BH1750(I2C& p_i2c, uint8_t addr);
 
    /** Get Illuminance, unit of Lux
      * @param none
      * @return Lux
      */
    float lux(void);
    WORD read();
 
    /** Set sensor sensitivity adjustment
      * @param sensitivity parameter
      * @return none
      */
    void set_sensitivity(uint8_t parameter);
 
    /** Set I2C clock frequency
      * @param freq.
      * @return none
      */
    void frequency(int hz);
 
    /** Power Up/Down
      * @param none
      * @return none
      */
    void power_up(void);
    void power_down(void);
 
protected:
    I2C  _i2c;
 
    void init(void);
 
private:
    uint8_t  BH1750_addr;
    uint8_t  dt[4];
    int8_t   sensitivity;
};
 
#endif      // BH1750_H
