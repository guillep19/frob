
#include "bh1750.h"
#include "serial.h"
 
BH1750::BH1750 (PinName p_sda, PinName p_scl) : _i2c(p_sda, p_scl) {
    BH1750_addr = BH1750_G_CHIP_ADDR;
    init();
}
 
BH1750::BH1750 (PinName p_sda, PinName p_scl, uint8_t addr) : _i2c(p_sda, p_scl) {
    BH1750_addr = addr;
    init();
}
 
BH1750::BH1750 (I2C& p_i2c) : _i2c(p_i2c) {
    BH1750_addr = BH1750_G_CHIP_ADDR;
    init();
}
 
BH1750::BH1750 (I2C& p_i2c, uint8_t addr) : _i2c(p_i2c) {
    BH1750_addr = addr;
    init();
}
 
/////////////// Read Lux from sensor //////////////////////
float BH1750::lux() {
    float lux;
 
    _i2c.read(BH1750_addr, (char *)dt, 2, false);
    lux = (float)(dt[0] << 8 | dt[1]);
    lux = lux / 1.2 * ((float)sensitivity/69);
    return lux;
}

WORD BH1750::read() {
    float value = lux();
    pc.printf("///////////////////////////// %.4f Lux\r\n", value);
    return 250;
}
 
/////////////// Initialize ////////////////////////////////
void BH1750::init() {
    _i2c.frequency(100000);
    power_up();
    sensitivity = SENS_1R00;
    set_sensitivity(sensitivity);
}
 
/////////////// Timing Register ///////////////////////////
void BH1750::set_sensitivity(uint8_t parameter) {
    if (parameter > SENS_3R68){
        parameter = SENS_3R68;
    } else if (parameter < SENS_0R45){
        parameter = SENS_0R45;
    }
    dt[0] = CMD_M_TIME_H | (parameter >> 5);    // Set High byte
    //_i2c.write((int)BH1750_addr, (char *)dt, 1, false);
    dt[0] = CMD_M_TIME_L | (parameter & 0x1f);  // Set Low byte
    //_i2c.write((int)BH1750_addr, (char *)dt, 1, false);
    dt[0] = CMD_C_H_RES_M;      // Measurement mode: High Resolution
    _i2c.write((int)BH1750_addr, (char *)dt, 1, false);
    wait_ms(240);   // need normal conversion time(120mS) x 2
}
 
/////////////// Power ON/OFF //////////////////////////////
void BH1750::power_up() {
    dt[0] = CMD_PWR_UP;
    _i2c.write((int)BH1750_addr, (char *)dt, 1, false);
}
 
void BH1750::power_down() {
    dt[0] = CMD_PWR_DWN;
    _i2c.write((int)BH1750_addr, (char *)dt, 1, false);
}
 
/////////////// I2C Freq. /////////////////////////////////
void BH1750::frequency(int hz) {
    _i2c.frequency(hz);
}
